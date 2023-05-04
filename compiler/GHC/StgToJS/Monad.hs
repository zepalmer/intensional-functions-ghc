{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | JS codegen state monad
module GHC.StgToJS.Monad
  ( runG
  , emitGlobal
  , addDependency
  , emitToplevel
  , emitStatic
  , emitClosureInfo
  , emitForeign
  , assertRtsStat
  , getSettings
  , globalOccs
  , setGlobalIdCache
  , getGlobalIdCache
  , GlobalOcc(..)
  -- * Group
  , modifyGroup
  , resetGroup
  )
where

import GHC.Prelude

import GHC.JS.Unsat.Syntax
import qualified GHC.JS.Syntax as Sat
import GHC.JS.Transform

import GHC.StgToJS.Types

import GHC.Unit.Module
import GHC.Stg.Syntax

import GHC.Types.SrcLoc
import GHC.Types.Id
import GHC.Types.Unique.FM
import GHC.Types.ForeignCall

import qualified Control.Monad.Trans.State.Strict as State
import GHC.Data.FastString
import GHC.Data.FastMutInt

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.List as L

runG :: StgToJSConfig -> Module -> UniqFM Id CgStgExpr -> G a -> IO a
runG config m unfloat action = State.evalStateT action =<< initState config m unfloat

initState :: StgToJSConfig -> Module -> UniqFM Id CgStgExpr -> IO GenState
initState config m unfloat = do
  id_gen <- newFastMutInt 1
  pure $ GenState
    { gsSettings  = config
    , gsModule    = m
    , gsId        = id_gen
    , gsIdents    = emptyIdCache
    , gsUnfloated = unfloat
    , gsGroup     = defaultGenGroupState
    , gsGlobal    = []
    }


modifyGroup :: (GenGroupState -> GenGroupState) -> G ()
modifyGroup f = State.modify mod_state
  where
    mod_state s = s { gsGroup = f (gsGroup s) }

-- | emit a global (for the current module) toplevel statement
emitGlobal :: JStat -> G ()
emitGlobal stat = State.modify (\s -> s { gsGlobal = stat : gsGlobal s })

-- | add a dependency on a particular symbol to the current group
addDependency :: OtherSymb -> G ()
addDependency symbol = modifyGroup mod_group
  where
    mod_group g = g { ggsExtraDeps = S.insert symbol (ggsExtraDeps g) }

-- | emit a top-level statement for the current binding group
emitToplevel :: JStat -> G ()
emitToplevel s = modifyGroup mod_group
  where
    mod_group g = g { ggsToplevelStats = s : ggsToplevelStats g}

-- | emit static data for the binding group
emitStatic :: FastString -> StaticVal -> Maybe Ident -> G ()
emitStatic ident val cc = modifyGroup mod_group
  where
    mod_group  g = g { ggsStatic = mod_static (ggsStatic g) }
    mod_static s = StaticInfo ident val cc : s

-- | add closure info in our binding group. all heap objects must have closure info
emitClosureInfo :: ClosureInfo -> G ()
emitClosureInfo ci = modifyGroup mod_group
  where
    mod_group g = g { ggsClosureInfo = ci : ggsClosureInfo g}

emitForeign :: Maybe RealSrcSpan
            -> FastString
            -> Safety
            -> CCallConv
            -> [FastString]
            -> FastString
            -> G ()
emitForeign mbSpan pat safety cconv arg_tys res_ty = modifyGroup mod_group
  where
    mod_group g = g { ggsForeignRefs = new_ref : ggsForeignRefs g }
    new_ref = ForeignJSRef spanTxt pat safety cconv arg_tys res_ty
    spanTxt = case mbSpan of
                -- TODO: Is there a better way to concatenate FastStrings?
                Just sp -> mkFastString $
                  unpackFS (srcSpanFile sp) ++
                  " " ++
                  show (srcSpanStartLine sp, srcSpanStartCol sp) ++
                  "-" ++
                  show (srcSpanEndLine sp, srcSpanEndCol sp)
                Nothing -> "<unknown>"






-- | start with a new binding group
resetGroup :: G ()
resetGroup = State.modify (\s -> s { gsGroup = defaultGenGroupState })

defaultGenGroupState :: GenGroupState
defaultGenGroupState = GenGroupState [] [] [] [] 0 S.empty emptyGlobalIdCache []

emptyGlobalIdCache :: GlobalIdCache
emptyGlobalIdCache = GlobalIdCache emptyUFM

emptyIdCache :: IdCache
emptyIdCache = IdCache M.empty



assertRtsStat :: G JStat -> G JStat
assertRtsStat stat = do
  s <- State.gets gsSettings
  if csAssertRts s then stat else pure mempty

getSettings :: G StgToJSConfig
getSettings = State.gets gsSettings

getGlobalIdCache :: G GlobalIdCache
getGlobalIdCache = State.gets (ggsGlobalIdCache . gsGroup)

setGlobalIdCache :: GlobalIdCache -> G ()
setGlobalIdCache v = State.modify (\s -> s { gsGroup = (gsGroup s) { ggsGlobalIdCache = v}})


data GlobalOcc = GlobalOcc
  { global_ident :: !Ident
  , global_id    :: !Id
  , global_count :: !Word
  }

-- | Return number of occurrences of every global id used in the given JStat.
-- Sort by increasing occurrence count.
globalOccs :: Sat.JStat -> G [GlobalOcc]
globalOccs jst = do
  GlobalIdCache gidc <- getGlobalIdCache
  -- build a map form Ident Unique to (Ident, Id, Count)
  let
    cmp_cnt g1 g2 = compare (global_count g1) (global_count g2)
    inc g1 g2 = g1 { global_count = global_count g1 + global_count g2 }
    go gids = \case
        []     -> -- return global Ids used locally sorted by increased use
                  L.sortBy cmp_cnt $ nonDetEltsUFM gids
        (i:is) ->
          -- check if the Id is global
          case lookupUFM gidc i of
            Nothing       -> go gids is
            Just (_k,gid) ->
              -- add it to the list of already found global ids. Increasing
              -- count by 1
              let g = GlobalOcc i gid 1
              in go (addToUFM_C inc gids i g) is

  pure $ go emptyUFM (identsS jst)
