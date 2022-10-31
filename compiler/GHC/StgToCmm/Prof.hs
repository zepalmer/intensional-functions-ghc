-----------------------------------------------------------------------------
--
-- Code generation for profiling
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Prof (
        initCostCentres, ccType, ccsType,
        mkCCostCentre, mkCCostCentreStack,

        -- infoTablePRov
        initInfoTableProv,

        -- Cost-centre Profiling
        dynProfHdr, profDynAlloc, profAlloc, staticProfHdr, initUpdFrameProf,
        enterCostCentreThunk, enterCostCentreFun,
        costCentreFrom,
        storeCurCCS,
        emitSetCCC,

        saveCurrentCostCentre, restoreCurrentCostCentre,

        -- Lag/drag/void stuff
        ldvEnter, ldvEnterClosure, ldvRecordCreate,

        ContainsSCCProfiling (..),
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Profile
import GHC.Platform.Profile.Class
import GHC.StgToCmm.Closure
import GHC.StgToCmm.Config
import GHC.StgToCmm.InfoTableProv
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Lit
import GHC.Runtime.Heap.Layout

import GHC.Cmm.Builder.Config
import GHC.Cmm.Graph
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.CLabel

import GHC.Types.CostCentre
import GHC.Types.IPE
import GHC.Types.ForeignStubs
import GHC.Data.FastString
import GHC.Unit.Module as Module
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Driver.CodeOutput ( ipInitCode )

import GHC.Utils.Encoding

import Control.Monad
import Data.Char       (ord)
import GHC.Utils.Monad (whenM)

-----------------------------------------------------------------------------
--
-- Cost-centre-stack Profiling
--
-----------------------------------------------------------------------------

-- Expression representing the current cost centre stack
ccsType :: Platform -> CmmType -- Type of a cost-centre stack
ccsType = bWord

ccType :: Platform -> CmmType -- Type of a cost centre
ccType = bWord

storeCurCCS :: CmmExpr -> CmmAGraph
storeCurCCS = mkAssign cccsReg

mkCCostCentre :: CostCentre -> CmmLit
mkCCostCentre cc = CmmLabel (mkCCLabel cc)

mkCCostCentreStack :: CostCentreStack -> CmmLit
mkCCostCentreStack ccs = CmmLabel (mkCCSLabel ccs)

costCentreFrom :: Platform
               -> CmmExpr        -- A closure pointer
               -> CmmExpr        -- The cost centre from that closure
costCentreFrom platform cl = CmmLoad (cmmOffsetB platform cl (pc_OFFSET_StgHeader_ccs (platformConstants platform))) (ccsType platform) NaturallyAligned

-- | The profiling header words in a static closure
staticProfHdr :: Profile -> CostCentreStack -> [CmmLit]
staticProfHdr profile ccs
  | profileIsProfiling profile = [mkCCostCentreStack ccs, staticLdvInit platform]
  | otherwise                  = []
  where platform = profilePlatform profile

-- | Profiling header words in a dynamic closure
dynProfHdr :: Profile -> CmmExpr -> [CmmExpr]
dynProfHdr profile ccs
  | profileIsProfiling profile = [ccs, dynLdvInit (profilePlatform profile)]
  | otherwise                  = []

-- | Initialise the profiling field of an update frame
initUpdFrameProf :: (ContainsPlatformProfile c, ContainsSCCProfiling c) => CmmExpr -> FCode' c ()
initUpdFrameProf frame
  = ifProfiling $        -- frame->header.prof.ccs = CCCS
    do platform <- getPlatform
       emitStore (cmmOffset platform frame (pc_OFFSET_StgHeader_ccs (platformConstants platform))) cccsExpr
        -- frame->header.prof.hp.rs = NULL (or frame-header.prof.hp.ldvw = 0)
        -- is unnecessary because it is not used anyhow.

---------------------------------------------------------------------------
--         Saving and restoring the current cost centre
---------------------------------------------------------------------------

{-        Note [Saving the current cost centre]
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The current cost centre is like a global register.  Like other
global registers, it's a caller-saves one.  But consider
        case (f x) of (p,q) -> rhs
Since 'f' may set the cost centre, we must restore it
before resuming rhs.  So we want code like this:
        local_cc = CCC  -- save
        r = f( x )
        CCC = local_cc  -- restore
That is, we explicitly "save" the current cost centre in
a LocalReg, local_cc; and restore it after the call. The
C-- infrastructure will arrange to save local_cc across the
call.

The same goes for join points;
        let j x = join-stuff
        in blah-blah
We want this kind of code:
        local_cc = CCC  -- save
        blah-blah
     J:
        CCC = local_cc  -- restore
-}

saveCurrentCostCentre :: FCode (Maybe LocalReg)
        -- Returns Nothing if profiling is off
saveCurrentCostCentre
  = do sccProfilingEnabled <- stgToCmmSCCProfiling <$> getStgToCmmConfig
       platform            <- getPlatform
       if not sccProfilingEnabled
           then return Nothing
           else do local_cc <- newTemp (ccType platform)
                   emitAssign (CmmLocal local_cc) cccsExpr
                   return (Just local_cc)

restoreCurrentCostCentre :: Maybe LocalReg -> FCode ()
restoreCurrentCostCentre Nothing
  = return ()
restoreCurrentCostCentre (Just local_cc)
  = emit (storeCurCCS (CmmReg (CmmLocal local_cc)))


-------------------------------------------------------------------------------
-- Recording allocation in a cost centre
-------------------------------------------------------------------------------

-- | Record the allocation of a closure.  The CmmExpr is the cost
-- centre stack to which to attribute the allocation.
profDynAlloc :: SMRep -> CmmExpr -> FCode ()
profDynAlloc rep ccs
  = ifProfiling $
    do profile <- getProfile
       let platform = profilePlatform profile
       profAlloc (mkIntExpr platform (heapClosureSizeW profile rep)) ccs

-- | Record the allocation of a closure (size is given by a CmmExpr)
-- The size must be in words, because the allocation counter in a CCS counts
-- in words.
profAlloc
  :: (ContainsPlatformProfile c, ContainsSCCProfiling c)
  => CmmExpr -> CmmExpr -> FCode' c ()
profAlloc words ccs
  = ifProfiling $
        do profile <- getProfile
           let platform = profilePlatform profile
           let alloc_rep = rEP_CostCentreStack_mem_alloc platform
           emit $ addToMemE alloc_rep
                       (cmmOffsetB platform ccs (pc_OFFSET_CostCentreStack_mem_alloc (platformConstants platform)))
                       (CmmMachOp (MO_UU_Conv (wordWidth platform) (typeWidth alloc_rep))
                           -- subtract the "profiling overhead", which is the
                           -- profiling header in a closure.
                           [CmmMachOp (mo_wordSub platform) [ words, mkIntExpr platform (profHdrSize profile)]]
                       )

-- -----------------------------------------------------------------------
-- Setting the current cost centre on entry to a closure

enterCostCentreThunk
  :: (ContainsPlatformProfile c, ContainsSCCProfiling c)
  => CmmExpr -> FCode' c ()
enterCostCentreThunk closure =
  ifProfiling $ do
      platform <- getPlatform
      emit $ storeCurCCS (costCentreFrom platform closure)

enterCostCentreFun :: CostCentreStack -> CmmExpr -> FCode ()
enterCostCentreFun ccs closure = ifProfiling $
    when (isCurrentCCS ccs) $
    do platform <- getPlatform
       emitRtsCall
         rtsUnitId
         (fsLit "enterFunCCS")
         [(baseExpr, AddrHint), (costCentreFrom platform closure, AddrHint)]
         False
       -- otherwise we have a top-level function, nothing to do

ifProfiling :: ContainsSCCProfiling c => FCode' c () -> FCode' c ()
ifProfiling = whenM (sccProfiling <$> getConfig)

---------------------------------------------------------------
--        Initialising Cost Centres & CCSs
---------------------------------------------------------------

initCostCentres :: CollectedCCs -> FCode ()
-- Emit the declarations
initCostCentres (local_CCs, singleton_CCSs)
  = ifProfiling $ do
      mapM_ emitCostCentreDecl local_CCs
      mapM_ emitCostCentreStackDecl singleton_CCSs


emitCostCentreDecl :: CostCentre -> FCode ()
emitCostCentreDecl cc = do
  { ctx      <- stgToCmmContext <$> getStgToCmmConfig
  ; platform <- getPlatform
  ; let is_caf | isCafCC cc = mkIntCLit platform (ord 'c') -- 'c' == is a CAF
               | otherwise  = zero platform
                        -- NB. bytesFS: we want the UTF-8 bytes here (#5559)
  ; label <- newByteStringCLit (bytesFS $ costCentreUserNameFS cc)
  ; modl  <- newByteStringCLit (bytesFS $ moduleNameFS
                                        $ moduleName
                                        $ cc_mod cc)
  ; loc <- newByteStringCLit $ utf8EncodeByteString $
                   renderWithContext ctx (ppr $! costCentreSrcSpan cc)
  ; let
     lits = [ zero platform,  -- StgInt ccID,
              label,          -- char *label,
              modl,           -- char *module,
              loc,            -- char *srcloc,
              zero64,         -- StgWord64 mem_alloc
              zero platform,  -- StgWord time_ticks
              is_caf,         -- StgInt is_caf
              zero platform   -- struct _CostCentre *link
            ]
  ; emitDataLits (mkCCLabel cc) lits
  }

emitCostCentreStackDecl :: CostCentreStack -> FCode ()
emitCostCentreStackDecl ccs
  = case maybeSingletonCCS ccs of
    Just cc ->
        do platform <- getPlatform
           let mk_lits cc = zero platform :
                            mkCCostCentre cc :
                            replicate (sizeof_ccs_words platform - 2) (zero platform)
                -- Note: to avoid making any assumptions about how the
                -- C compiler (that compiles the RTS, in particular) does
                -- layouts of structs containing long-longs, simply
                -- pad out the struct with zero words until we hit the
                -- size of the overall struct (which we get via DerivedConstants.h)
           emitDataLits (mkCCSLabel ccs) (mk_lits cc)
    Nothing -> pprPanic "emitCostCentreStackDecl" (ppr ccs)

zero :: Platform -> CmmLit
zero platform = mkIntCLit platform 0
zero64 :: CmmLit
zero64 = CmmInt 0 W64

sizeof_ccs_words :: Platform -> Int
sizeof_ccs_words platform
    -- round up to the next word.
  | ms == 0   = ws
  | otherwise = ws + 1
  where
   (ws,ms) = pc_SIZEOF_CostCentreStack (platformConstants platform) `divMod` platformWordSizeInBytes platform

-- | Emit info-table provenance declarations
initInfoTableProv ::  [CmmInfoTable] -> InfoTableProvMap -> FCode CStub
initInfoTableProv infos itmap
  = do
       cfg <- getStgToCmmConfig
       let ents       = convertInfoProvMap infos this_mod itmap
           info_table = stgToCmmInfoTableMap cfg
           platform   = stgToCmmPlatform     cfg
           this_mod   = stgToCmmThisModule   cfg

       case ents of
         [] -> return mempty
         _  -> do
           -- Emit IPE buffer
           emitIpeBufferListNode this_mod ents

           -- Create the C stub which initialises the IPE map
           return (ipInitCode info_table platform this_mod)

-- ---------------------------------------------------------------------------
-- Set the current cost centre stack

emitSetCCC :: CostCentre -> Bool -> Bool -> FCode ()
emitSetCCC cc tick push = ifProfiling $
  do platform <- getPlatform
     tmp      <- newTemp (ccsType platform)
     pushCostCentre tmp cccsExpr cc
     when tick $ emit (bumpSccCount platform (CmmReg (CmmLocal tmp)))
     when push $ emit (storeCurCCS (CmmReg (CmmLocal tmp)))

pushCostCentre :: LocalReg -> CmmExpr -> CostCentre -> FCode ()
pushCostCentre result ccs cc
  = emitRtsCallWithResult result AddrHint
        rtsUnitId
        (fsLit "pushCostCentre") [(ccs,AddrHint),
                                (CmmLit (mkCCostCentre cc), AddrHint)]
        False

bumpSccCount :: Platform -> CmmExpr -> CmmAGraph
bumpSccCount platform ccs
  = addToMem (rEP_CostCentreStack_scc_count platform)
         (cmmOffsetB platform ccs (pc_OFFSET_CostCentreStack_scc_count (platformConstants platform))) 1

-----------------------------------------------------------------------------
--
--                Lag/drag/void stuff
--
-----------------------------------------------------------------------------

--
-- Initial value for the LDV field in a static closure
--
staticLdvInit :: Platform -> CmmLit
staticLdvInit = zeroCLit

--
-- Initial value of the LDV field in a dynamic closure
--
dynLdvInit :: Platform -> CmmExpr
dynLdvInit platform =     -- (era << LDV_SHIFT) | LDV_STATE_CREATE
  CmmMachOp (mo_wordOr platform) [
      CmmMachOp (mo_wordShl platform) [loadEra platform, mkIntExpr platform (pc_LDV_SHIFT (platformConstants platform))],
      CmmLit (mkWordCLit platform (pc_ILDV_STATE_CREATE (platformConstants platform)))
  ]

--
-- Initialise the LDV word of a new closure
--
ldvRecordCreate :: ContainsPlatformProfile c => CmmExpr -> FCode' c ()
ldvRecordCreate closure = do
  platform <- getPlatform
  emit $ mkStore (ldvWord platform closure) (dynLdvInit platform)

--
-- | Called when a closure is entered, marks the closure as having
-- been "used".  The closure is not an "inherently used" one.  The
-- closure is not @IND@ because that is not considered for LDV profiling.
--
ldvEnterClosure :: ClosureInfo -> CmmReg -> FCode ()
ldvEnterClosure closure_info node_reg = do
    platform <- getPlatform
    let tag = funTag platform closure_info
    -- don't forget to subtract node's tag
    ldvEnter (cmmOffsetB platform (CmmReg node_reg) (-tag))

ldvEnter :: (ContainsPlatformProfile c, ContainsSCCProfiling c) => CmmExpr -> FCode' c ()
-- Argument is a closure pointer
ldvEnter cl_ptr = do
    platform <- getPlatform
    let constants = platformConstants platform
        -- don't forget to subtract node's tag
        ldv_wd = ldvWord platform cl_ptr
        new_ldv_wd = cmmOrWord platform
                        (cmmAndWord platform (cmmLoadBWord platform ldv_wd)
                                             (CmmLit (mkWordCLit platform (pc_ILDV_CREATE_MASK constants))))
                        (cmmOrWord platform (loadEra platform) (CmmLit (mkWordCLit platform (pc_ILDV_STATE_USE constants))))
    ifProfiling $
         -- if (era > 0) {
         --    LDVW((c)) = (LDVW((c)) & LDV_CREATE_MASK) |
         --                era | LDV_STATE_USE }
        emit =<< mkCmmIfThenElse (CmmMachOp (mo_wordUGt platform) [loadEra platform, CmmLit (zeroCLit platform)])
                     (mkStore ldv_wd new_ldv_wd)
                     mkNop

loadEra :: Platform -> CmmExpr
loadEra platform = CmmMachOp (MO_UU_Conv (cIntWidth platform) (wordWidth platform))
    [CmmLoad (mkLblExpr (mkRtsCmmDataLabel (fsLit "era")))
             (cInt platform)
             NaturallyAligned]

-- | Takes the address of a closure, and returns
-- the address of the LDV word in the closure
ldvWord :: Platform -> CmmExpr -> CmmExpr
ldvWord platform closure_ptr
    = cmmOffsetB platform closure_ptr (pc_OFFSET_StgHeader_ldvw (platformConstants platform))

-----------------------------------------------------------------------------
--
--                Utilities
--
-----------------------------------------------------------------------------

class ContainsSCCProfiling c where
  sccProfiling :: c -> Bool

instance ContainsSCCProfiling CmmBuilderConfig where
  sccProfiling = cmmBuilderSCCProfiling

instance ContainsSCCProfiling StgToCmmConfig where
  sccProfiling = stgToCmmSCCProfiling
