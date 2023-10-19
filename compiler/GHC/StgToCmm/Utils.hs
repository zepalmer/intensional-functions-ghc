{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
--
-- Code generator utilities; mostly monadic
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Utils (
        cgLit, mkSimpleLit,
        emitDataLits, emitRODataLits,
        emitDataCon,
        emitRtsCall, emitRtsCallWithResult, emitRtsCallGen,
        assignTemp, newTemp,

        newUnboxedTupleRegs,

        emitMultiAssign, emitCmmLitSwitch, emitSwitch,

        tagToClosure, mkTaggedObjectLoad,

        callerSaves, callerSaveVolatileRegs, get_GlobalReg_addr,
        callerSaveGlobalReg, callerRestoreGlobalReg,

        cmmAndWord, cmmOrWord, cmmNegate, cmmEqWord, cmmNeWord,
        cmmUGtWord, cmmSubWord, cmmMulWord, cmmAddWord, cmmUShrWord,
        cmmOffsetExprW, cmmOffsetExprB,
        cmmRegOffW, cmmRegOffB,
        cmmLabelOffW, cmmLabelOffB,
        cmmOffsetW, cmmOffsetB,
        cmmOffsetLitW, cmmOffsetLitB,
        cmmLoadIndexW,
        cmmConstrTag1,

        cmmUntag, cmmIsTagged,

        addToMem, addToMemE, addToMemLblE, addToMemLbl,
        newStringCLit, newByteStringCLit,

        -- * Update remembered set operations
        whenUpdRemSetEnabled,
        emitUpdRemSetPush,
        emitUpdRemSetPushThunk,

        convertInfoProvMap, cmmInfoTableToInfoProvEnt
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Platform
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Closure
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.Graph as CmmGraph
import GHC.Platform.Regs
import GHC.Cmm.CLabel
import GHC.Cmm.Utils
import GHC.Cmm.Switch
import GHC.StgToCmm.CgUtils

import GHC.Types.ForeignCall
import GHC.Types.Id.Info
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Runtime.Heap.Layout
import GHC.Unit
import GHC.Types.Literal
import GHC.Data.Graph.Directed
import GHC.Utils.Misc
import GHC.Types.Unique
import GHC.Types.Unique.Supply (MonadUnique(..))
import GHC.Driver.Session
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.RepType
import GHC.Types.CostCentre
import GHC.Types.IPE

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import Data.Char
import Data.List (sortBy)
import Data.Ord
import GHC.Types.Unique.Map
import Data.Maybe
import GHC.Driver.Ppr
import qualified Data.List.NonEmpty as NE
import GHC.Core.DataCon
import GHC.Types.Unique.FM
import GHC.Data.Maybe
import Control.Monad

-------------------------------------------------------------------------
--
--      Literals
--
-------------------------------------------------------------------------

cgLit :: Literal -> FCode CmmLit
cgLit (LitString s) = newByteStringCLit s
 -- not unpackFS; we want the UTF-8 byte stream.
cgLit other_lit     = do platform <- getPlatform
                         return (mkSimpleLit platform other_lit)

mkSimpleLit :: Platform -> Literal -> CmmLit
mkSimpleLit platform = \case
   (LitChar   c)                -> CmmInt (fromIntegral (ord c))
                                          (wordWidth platform)
   LitNullAddr                  -> zeroCLit platform
   (LitNumber LitNumInt i)      -> CmmInt i (wordWidth platform)
   (LitNumber LitNumInt8 i)     -> CmmInt i W8
   (LitNumber LitNumInt16 i)    -> CmmInt i W16
   (LitNumber LitNumInt32 i)    -> CmmInt i W32
   (LitNumber LitNumInt64 i)    -> CmmInt i W64
   (LitNumber LitNumWord i)     -> CmmInt i (wordWidth platform)
   (LitNumber LitNumWord8 i)    -> CmmInt i W8
   (LitNumber LitNumWord16 i)   -> CmmInt i W16
   (LitNumber LitNumWord32 i)   -> CmmInt i W32
   (LitNumber LitNumWord64 i)   -> CmmInt i W64
   (LitFloat r)                 -> CmmFloat r W32
   (LitDouble r)                -> CmmFloat r W64
   (LitLabel fs ms fod)
     -> let -- TODO: Literal labels might not actually be in the current package...
            labelSrc = ForeignLabelInThisPackage
        in CmmLabel (mkForeignLabel fs ms labelSrc fod)
   -- NB: LitRubbish should have been lowered in "CoreToStg"
   other -> pprPanic "mkSimpleLit" (ppr other)

--------------------------------------------------------------------------
--
-- Incrementing a memory location
--
--------------------------------------------------------------------------

addToMemLbl :: CmmType -> CLabel -> Int -> CmmAGraph
addToMemLbl rep lbl n = addToMem rep (CmmLit (CmmLabel lbl)) n

addToMemLblE :: CmmType -> CLabel -> CmmExpr -> CmmAGraph
addToMemLblE rep lbl = addToMemE rep (CmmLit (CmmLabel lbl))

-- | @addToMem rep ptr n@ adds @n@ to the integer pointed-to by @ptr@.
addToMem :: CmmType     -- rep of the counter
         -> CmmExpr     -- Naturally-aligned address
         -> Int         -- What to add (a word)
         -> CmmAGraph
addToMem rep ptr n = addToMemE rep ptr (CmmLit (CmmInt (toInteger n) (typeWidth rep)))

-- | @addToMemE rep ptr n@ adds @n@ to the integer pointed-to by @ptr@.
addToMemE :: CmmType    -- rep of the counter
          -> CmmExpr    -- Naturally-aligned address
          -> CmmExpr    -- What to add (a word-typed expression)
          -> CmmAGraph
addToMemE rep ptr n
  = mkStore ptr (CmmMachOp (MO_Add (typeWidth rep)) [CmmLoad ptr rep NaturallyAligned, n])


-------------------------------------------------------------------------
--
--      Loading a field from an object,
--      where the object pointer is itself tagged
--
-------------------------------------------------------------------------

mkTaggedObjectLoad
  :: Platform -> LocalReg -> LocalReg -> ByteOff -> DynTag -> CmmAGraph
-- (loadTaggedObjectField reg base off tag) generates assignment
--      reg = bitsK[ base + off - tag ]
-- where K is fixed by 'reg'
mkTaggedObjectLoad platform reg base offset tag
  = mkAssign (CmmLocal reg)
             (CmmLoad (cmmOffsetB platform
                                  (CmmReg (CmmLocal base))
                                  (offset - tag))
                      (localRegType reg)
                      NaturallyAligned)

-------------------------------------------------------------------------
--
--      Converting a closure tag to a closure for enumeration types
--      (this is the implementation of tagToEnum#).
--
-------------------------------------------------------------------------

tagToClosure :: Platform -> TyCon -> CmmExpr -> CmmExpr
tagToClosure platform tycon tag
  = cmmLoadBWord platform (cmmOffsetExprW platform closure_tbl tag)
  where closure_tbl = CmmLit (CmmLabel lbl)
        lbl = mkClosureTableLabel (tyConName tycon) NoCafRefs

-------------------------------------------------------------------------
--
--      Conditionals and rts calls
--
-------------------------------------------------------------------------

emitRtsCall :: UnitId -> FastString -> [(CmmExpr,ForeignHint)] -> Bool -> FCode ()
emitRtsCall pkg fun args safe = emitRtsCallGen [] (mkCmmCodeLabel pkg fun) args safe

emitRtsCallWithResult :: LocalReg -> ForeignHint -> UnitId -> FastString
        -> [(CmmExpr,ForeignHint)] -> Bool -> FCode ()
emitRtsCallWithResult res hint pkg fun args safe
   = emitRtsCallGen [(res,hint)] (mkCmmCodeLabel pkg fun) args safe

-- Make a call to an RTS C procedure
emitRtsCallGen
   :: [(LocalReg,ForeignHint)]
   -> CLabel
   -> [(CmmExpr,ForeignHint)]
   -> Bool -- True <=> CmmSafe call
   -> FCode ()
emitRtsCallGen res lbl args safe
  = do { platform <- targetPlatform <$> getDynFlags
       ; updfr_off <- getUpdFrameOff
       ; let (caller_save, caller_load) = callerSaveVolatileRegs platform
       ; emit caller_save
       ; call updfr_off
       ; emit caller_load }
  where
    call updfr_off =
      if safe then
        emit =<< mkCmmCall fun_expr res' args' updfr_off
      else do
        let conv = ForeignConvention CCallConv arg_hints res_hints CmmMayReturn
        emit $ mkUnsafeCall (ForeignTarget fun_expr conv) res' args'
    (args', arg_hints) = unzip args
    (res',  res_hints) = unzip res
    fun_expr = mkLblExpr lbl


-----------------------------------------------------------------------------
--
--      Caller-Save Registers
--
-----------------------------------------------------------------------------

-- Here we generate the sequence of saves/restores required around a
-- foreign call instruction.

-- TODO: reconcile with includes/Regs.h
--  * Regs.h claims that BaseReg should be saved last and loaded first
--    * This might not have been tickled before since BaseReg is callee save
--  * Regs.h saves SparkHd, ParkT1, SparkBase and SparkLim
--
-- This code isn't actually used right now, because callerSaves
-- only ever returns true in the current universe for registers NOT in
-- system_regs (just do a grep for CALLER_SAVES in
-- includes/stg/MachRegs.h).  It's all one giant no-op, and for
-- good reason: having to save system registers on every foreign call
-- would be very expensive, so we avoid assigning them to those
-- registers when we add support for an architecture.
--
-- Note that the old code generator actually does more work here: it
-- also saves other global registers.  We can't (nor want) to do that
-- here, as we don't have liveness information.  And really, we
-- shouldn't be doing the workaround at this point in the pipeline, see
-- Note [Register parameter passing] and the ToDo on CmmCall in
-- "GHC.Cmm.Node".  Right now the workaround is to avoid inlining across
-- unsafe foreign calls in GHC.Cmm.Sink, but this is strictly
-- temporary.
callerSaveVolatileRegs :: Platform -> (CmmAGraph, CmmAGraph)
callerSaveVolatileRegs platform = (caller_save, caller_load)
  where
    caller_save = catAGraphs (map (callerSaveGlobalReg    platform) regs_to_save)
    caller_load = catAGraphs (map (callerRestoreGlobalReg platform) regs_to_save)

    system_regs = [ Sp,SpLim,Hp,HpLim,CCCS,CurrentTSO,CurrentNursery
                    {- ,SparkHd,SparkTl,SparkBase,SparkLim -}
                  , BaseReg ]

    regs_to_save = filter (callerSaves platform) system_regs

callerSaveGlobalReg :: Platform -> GlobalReg -> CmmAGraph
callerSaveGlobalReg platform reg
    = mkStore (get_GlobalReg_addr platform reg) (CmmReg (CmmGlobal reg))

callerRestoreGlobalReg :: Platform -> GlobalReg -> CmmAGraph
callerRestoreGlobalReg platform reg
    = mkAssign (CmmGlobal reg)
               (CmmLoad (get_GlobalReg_addr platform reg)
                        (globalRegType platform reg)
                        NaturallyAligned)


-------------------------------------------------------------------------
--
--      Strings generate a top-level data block
--
-------------------------------------------------------------------------

-- | Emit a data-segment data block
emitDataLits :: CLabel -> [CmmLit] -> FCode ()
emitDataLits lbl lits = emitDecl (mkDataLits (Section Data lbl) lbl lits)

-- | Emit a read-only data block
emitRODataLits :: CLabel -> [CmmLit] -> FCode ()
emitRODataLits lbl lits = emitDecl (mkRODataLits lbl lits)

emitDataCon :: CLabel -> CmmInfoTable -> CostCentreStack -> [CmmLit] -> FCode ()
emitDataCon lbl itbl ccs payload =
  emitDecl (CmmData (Section Data lbl) (CmmStatics lbl itbl ccs payload))

newStringCLit :: String -> FCode CmmLit
-- Make a global definition for the string,
-- and return its label
newStringCLit str = newByteStringCLit (BS8.pack str)

newByteStringCLit :: ByteString -> FCode CmmLit
newByteStringCLit bytes
  = do  { uniq <- newUnique
        ; let (lit, decl) = mkByteStringCLit (mkStringLitLabel uniq) bytes
        ; emitDecl decl
        ; return lit }

-------------------------------------------------------------------------
--
--      Assigning expressions to temporaries
--
-------------------------------------------------------------------------

assignTemp :: CmmExpr -> FCode LocalReg
-- Make sure the argument is in a local register.
-- We don't bother being particularly aggressive with avoiding
-- unnecessary local registers, since we can rely on a later
-- optimization pass to inline as necessary (and skipping out
-- on things like global registers can be a little dangerous
-- due to them being trashed on foreign calls--though it means
-- the optimization pass doesn't have to do as much work)
assignTemp (CmmReg (CmmLocal reg)) = return reg
assignTemp e = do { platform <- getPlatform
                  ; reg <- newTemp (cmmExprType platform e)
                  ; emitAssign (CmmLocal reg) e
                  ; return reg }

newTemp :: MonadUnique m => CmmType -> m LocalReg
newTemp rep = do { uniq <- getUniqueM
                 ; return (LocalReg uniq rep) }

newUnboxedTupleRegs :: Type -> FCode ([LocalReg], [ForeignHint])
-- Choose suitable local regs to use for the components
-- of an unboxed tuple that we are about to return to
-- the Sequel.  If the Sequel is a join point, using the
-- regs it wants will save later assignments.
newUnboxedTupleRegs res_ty
  = ASSERT( isUnboxedTupleType res_ty )
    do  { platform <- getPlatform
        ; sequel <- getSequel
        ; regs <- choose_regs platform sequel
        ; ASSERT( regs `equalLength` reps )
          return (regs, map primRepForeignHint reps) }
  where
    reps = typePrimRep res_ty
    choose_regs _ (AssignTo regs _) = return regs
    choose_regs platform _          = mapM (newTemp . primRepCmmType platform) reps



-------------------------------------------------------------------------
--      emitMultiAssign
-------------------------------------------------------------------------

emitMultiAssign :: [LocalReg] -> [CmmExpr] -> FCode ()
-- Emit code to perform the assignments in the
-- input simultaneously, using temporary variables when necessary.

type Key  = Int
type Vrtx = (Key, Stmt) -- Give each vertex a unique number,
                        -- for fast comparison
type Stmt = (LocalReg, CmmExpr) -- r := e

-- We use the strongly-connected component algorithm, in which
--      * the vertices are the statements
--      * an edge goes from s1 to s2 iff
--              s1 assigns to something s2 uses
--        that is, if s1 should *follow* s2 in the final order

emitMultiAssign []    []    = return ()
emitMultiAssign [reg] [rhs] = emitAssign (CmmLocal reg) rhs
emitMultiAssign regs rhss   = do
  platform <- getPlatform
  ASSERT2( equalLength regs rhss, ppr regs $$ pdoc platform rhss )
    unscramble platform ([1..] `zip` (regs `zip` rhss))

unscramble :: Platform -> [Vrtx] -> FCode ()
unscramble platform vertices = mapM_ do_component components
  where
        edges :: [ Node Key Vrtx ]
        edges = [ DigraphNode vertex key1 (edges_from stmt1)
                | vertex@(key1, stmt1) <- vertices ]

        edges_from :: Stmt -> [Key]
        edges_from stmt1 = [ key2 | (key2, stmt2) <- vertices,
                                    stmt1 `mustFollow` stmt2 ]

        components :: [SCC Vrtx]
        components = stronglyConnCompFromEdgedVerticesUniq edges

        -- do_components deal with one strongly-connected component
        -- Not cyclic, or singleton?  Just do it
        do_component :: SCC Vrtx -> FCode ()
        do_component (AcyclicSCC (_,stmt))  = mk_graph stmt
        do_component (CyclicSCC [])         = panic "do_component"
        do_component (CyclicSCC [(_,stmt)]) = mk_graph stmt

                -- Cyclic?  Then go via temporaries.  Pick one to
                -- break the loop and try again with the rest.
        do_component (CyclicSCC ((_,first_stmt) : rest)) = do
            u <- newUnique
            let (to_tmp, from_tmp) = split u first_stmt
            mk_graph to_tmp
            unscramble platform rest
            mk_graph from_tmp

        split :: Unique -> Stmt -> (Stmt, Stmt)
        split uniq (reg, rhs)
          = ((tmp, rhs), (reg, CmmReg (CmmLocal tmp)))
          where
            rep = cmmExprType platform rhs
            tmp = LocalReg uniq rep

        mk_graph :: Stmt -> FCode ()
        mk_graph (reg, rhs) = emitAssign (CmmLocal reg) rhs

        mustFollow :: Stmt -> Stmt -> Bool
        (reg, _) `mustFollow` (_, rhs) = regUsedIn platform (CmmLocal reg) rhs

-------------------------------------------------------------------------
--      mkSwitch
-------------------------------------------------------------------------


emitSwitch :: CmmExpr                      -- Tag to switch on
           -> [(ConTagZ, CmmAGraphScoped)] -- Tagged branches
           -> Maybe CmmAGraphScoped        -- Default branch (if any)
           -> ConTagZ -> ConTagZ           -- Min and Max possible values;
                                           -- behaviour outside this range is
                                           -- undefined
           -> FCode ()

-- First, two rather common cases in which there is no work to do
emitSwitch _ []         (Just code) _ _ = emit (fst code)
emitSwitch _ [(_,code)] Nothing     _ _ = emit (fst code)

-- Right, off we go
emitSwitch tag_expr branches mb_deflt lo_tag hi_tag = do
    join_lbl      <- newBlockId
    mb_deflt_lbl  <- label_default join_lbl mb_deflt
    branches_lbls <- label_branches join_lbl branches
    tag_expr'     <- assignTemp' tag_expr

    -- Sort the branches before calling mk_discrete_switch
    let branches_lbls' = [ (fromIntegral i, l) | (i,l) <- sortBy (comparing fst) branches_lbls ]
    let range = (fromIntegral lo_tag, fromIntegral hi_tag)

    emit $ mk_discrete_switch False tag_expr' branches_lbls' mb_deflt_lbl range

    emitLabel join_lbl

mk_discrete_switch :: Bool -- ^ Use signed comparisons
          -> CmmExpr
          -> [(Integer, BlockId)]
          -> Maybe BlockId
          -> (Integer, Integer)
          -> CmmAGraph

-- SINGLETON TAG RANGE: no case analysis to do
mk_discrete_switch _ _tag_expr [(tag, lbl)] _ (lo_tag, hi_tag)
  | lo_tag == hi_tag
  = ASSERT( tag == lo_tag )
    mkBranch lbl

-- SINGLETON BRANCH, NO DEFAULT: no case analysis to do
mk_discrete_switch _ _tag_expr [(_tag,lbl)] Nothing _
  = mkBranch lbl
        -- The simplifier might have eliminated a case
        --       so we may have e.g. case xs of
        --                               [] -> e
        -- In that situation we can be sure the (:) case
        -- can't happen, so no need to test

-- SOMETHING MORE COMPLICATED: defer to GHC.Cmm.Switch.Implement
-- See Note [Cmm Switches, the general plan] in GHC.Cmm.Switch
mk_discrete_switch signed tag_expr branches mb_deflt range
  = mkSwitch tag_expr $ mkSwitchTargets signed range mb_deflt (M.fromList branches)

divideBranches :: Ord a => [(a,b)] -> ([(a,b)], a, [(a,b)])
divideBranches branches = (lo_branches, mid, hi_branches)
  where
    -- 2 branches => n_branches `div` 2 = 1
    --            => branches !! 1 give the *second* tag
    -- There are always at least 2 branches here
    (mid,_) = branches !! (length branches `div` 2)
    (lo_branches, hi_branches) = span is_lo branches
    is_lo (t,_) = t < mid

--------------
emitCmmLitSwitch :: CmmExpr                    -- Tag to switch on
               -> [(Literal, CmmAGraphScoped)] -- Tagged branches
               -> CmmAGraphScoped              -- Default branch (always)
               -> FCode ()                     -- Emit the code
emitCmmLitSwitch _scrut []       deflt = emit $ fst deflt
emitCmmLitSwitch scrut  branches deflt = do
    scrut' <- assignTemp' scrut
    join_lbl <- newBlockId
    deflt_lbl <- label_code join_lbl deflt
    branches_lbls <- label_branches join_lbl branches

    platform <- getPlatform
    let cmm_ty = cmmExprType platform scrut
        rep = typeWidth cmm_ty

    -- We find the necessary type information in the literals in the branches
    let signed = case head branches of
                    (LitNumber nt _, _) -> litNumIsSigned nt
                    _ -> False

    let range | signed    = (platformMinInt platform, platformMaxInt platform)
              | otherwise = (0, platformMaxWord platform)

    if isFloatType cmm_ty
    then emit =<< mk_float_switch rep scrut' deflt_lbl noBound branches_lbls
    else emit $ mk_discrete_switch
        signed
        scrut'
        [(litValue lit,l) | (lit,l) <- branches_lbls]
        (Just deflt_lbl)
        range
    emitLabel join_lbl

-- | lower bound (inclusive), upper bound (exclusive)
type LitBound = (Maybe Literal, Maybe Literal)

noBound :: LitBound
noBound = (Nothing, Nothing)

mk_float_switch :: Width -> CmmExpr -> BlockId
              -> LitBound
              -> [(Literal,BlockId)]
              -> FCode CmmAGraph
mk_float_switch rep scrut deflt _bounds [(lit,blk)]
  = do platform <- getPlatform
       return $ mkCbranch (cond platform) deflt blk Nothing
  where
    cond platform = CmmMachOp ne [scrut, CmmLit cmm_lit]
      where
        cmm_lit = mkSimpleLit platform lit
        ne      = MO_F_Ne rep

mk_float_switch rep scrut deflt_blk_id (lo_bound, hi_bound) branches
  = do platform <- getPlatform
       lo_blk <- mk_float_switch rep scrut deflt_blk_id bounds_lo lo_branches
       hi_blk <- mk_float_switch rep scrut deflt_blk_id bounds_hi hi_branches
       mkCmmIfThenElse (cond platform) lo_blk hi_blk
  where
    (lo_branches, mid_lit, hi_branches) = divideBranches branches

    bounds_lo = (lo_bound, Just mid_lit)
    bounds_hi = (Just mid_lit, hi_bound)

    cond platform = CmmMachOp lt [scrut, CmmLit cmm_lit]
      where
        cmm_lit = mkSimpleLit platform mid_lit
        lt      = MO_F_Lt rep


--------------
label_default :: BlockId -> Maybe CmmAGraphScoped -> FCode (Maybe BlockId)
label_default _ Nothing
  = return Nothing
label_default join_lbl (Just code)
  = do lbl <- label_code join_lbl code
       return (Just lbl)

--------------
label_branches :: BlockId -> [(a,CmmAGraphScoped)] -> FCode [(a,BlockId)]
label_branches _join_lbl []
  = return []
label_branches join_lbl ((tag,code):branches)
  = do lbl <- label_code join_lbl code
       branches' <- label_branches join_lbl branches
       return ((tag,lbl):branches')

--------------
label_code :: BlockId -> CmmAGraphScoped -> FCode BlockId
--  label_code J code
--      generates
--  [L: code; goto J]
-- and returns L
label_code join_lbl (code,tsc) = do
    lbl <- newBlockId
    emitOutOfLine lbl (code CmmGraph.<*> mkBranch join_lbl, tsc)
    return lbl

--------------
assignTemp' :: CmmExpr -> FCode CmmExpr
assignTemp' e
  | isTrivialCmmExpr e = return e
  | otherwise = do
       platform <- getPlatform
       lreg <- newTemp (cmmExprType platform e)
       let reg = CmmLocal lreg
       emitAssign reg e
       return (CmmReg reg)


---------------------------------------------------------------------------
-- Pushing to the update remembered set
---------------------------------------------------------------------------

whenUpdRemSetEnabled :: FCode a -> FCode ()
whenUpdRemSetEnabled code = do
    platform <- getPlatform
    do_it <- getCode code
    let
      enabled = cmmLoadBWord platform (CmmLit $ CmmLabel mkNonmovingWriteBarrierEnabledLabel)
      zero = zeroExpr platform
      is_enabled = cmmNeWord platform enabled zero
    the_if <- mkCmmIfThenElse' is_enabled do_it mkNop (Just False)
    emit the_if

-- | Emit code to add an entry to a now-overwritten pointer to the update
-- remembered set.
emitUpdRemSetPush :: CmmExpr   -- ^ value of pointer which was overwritten
                  -> FCode ()
emitUpdRemSetPush ptr =
    emitRtsCall
      rtsUnitId
      (fsLit "updateRemembSetPushClosure_")
      [(CmmReg (CmmGlobal BaseReg), AddrHint),
       (ptr, AddrHint)]
      False

emitUpdRemSetPushThunk :: CmmExpr -- ^ the thunk
                       -> FCode ()
emitUpdRemSetPushThunk ptr =
    emitRtsCall
      rtsUnitId
      (fsLit "updateRemembSetPushThunk_")
      [(CmmReg (CmmGlobal BaseReg), AddrHint),
       (ptr, AddrHint)]
      False

-- | A bare bones InfoProvEnt for things which don't have a good source location
cmmInfoTableToInfoProvEnt :: Module -> CmmInfoTable -> InfoProvEnt
cmmInfoTableToInfoProvEnt this_mod cmit =
    let cl = cit_lbl cmit
        cn  = rtsClosureType (cit_rep cmit)
    in InfoProvEnt cl cn "" this_mod Nothing

-- | Convert source information collected about identifiers in 'GHC.STG.Debug'
-- to entries suitable for placing into the info table provenenance table.
convertInfoProvMap :: DynFlags -> [CmmInfoTable] -> Module -> InfoTableProvMap -> [InfoProvEnt]
convertInfoProvMap dflags defns this_mod (InfoTableProvMap (UniqMap dcenv) denv) =
  map (\cmit ->
    let cl = cit_lbl cmit
        cn  = rtsClosureType (cit_rep cmit)

        tyString :: Outputable a => a -> String
        tyString t = showPpr dflags t

        lookupClosureMap :: Maybe InfoProvEnt
        lookupClosureMap = case hasHaskellName cl >>= lookupUniqMap denv of
                                Just (ty, mbspan) -> Just (InfoProvEnt cl cn (tyString ty) this_mod mbspan)
                                Nothing -> Nothing

        lookupDataConMap = do
            UsageSite _ n <- hasIdLabelInfo cl >>= getConInfoTableLocation
            -- This is a bit grimy, relies on the DataCon and Name having the same Unique, which they do
            (dc, ns) <- (hasHaskellName cl >>= lookupUFM_Directly dcenv . getUnique)
            -- Lookup is linear but lists will be small (< 100)
            return $ InfoProvEnt cl cn (tyString (dataConTyCon dc)) this_mod (join $ lookup n (NE.toList ns))

        -- This catches things like prim closure types and anything else which doesn't have a
        -- source location
        simpleFallback = cmmInfoTableToInfoProvEnt this_mod cmit

    in fromMaybe simpleFallback (lookupDataConMap `firstJust` lookupClosureMap)) defns
