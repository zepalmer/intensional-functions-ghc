{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Deps
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- Module to calculate the transitive dependencies of a module
-----------------------------------------------------------------------------

module GHC.StgToJS.Deps
  ( genDependencyData
  )
where

import GHC.Prelude

import GHC.StgToJS.Object
import GHC.StgToJS.Types
import GHC.StgToJS.Ids

import GHC.JS.Unsat.Syntax

import GHC.Types.Id
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Name

import GHC.Unit.Module

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.FastString

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Array
import Data.Either
import Control.Monad

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

data DependencyDataCache = DDC
  { ddcModule :: !(IntMap Unit)        -- ^ Unique Module -> Unit
  , ddcId     :: !(IntMap ExportedFun) -- ^ Unique Id     -> ExportedFun (only to other modules)
  , ddcOther  :: !(Map OtherSymb ExportedFun)
  }

-- | Generate module dependency data
--
-- Generate the object's dependency data, taking care that package and module names
-- are only stored once
genDependencyData
  :: HasDebugCallStack
  => Module
  -> [LinkableUnit]
  -> G BlockInfo
genDependencyData mod units = do
    ds <- evalStateT (mapM (uncurry oneDep) blocks)
                     (DDC IM.empty IM.empty M.empty)
    return $ BlockInfo
      { bi_module     = mod
      , bi_must_link  = IS.fromList [ n | (n, _, True, _) <- ds ]
      , bi_exports    = M.fromList $ (\(n,_,_,es) -> map (,n) es) =<< ds
      , bi_block_deps = listArray (0, length blocks-1) (map (\(_,deps,_,_) -> deps) ds)
      }
  where
      -- Id -> Block
      unitIdExports :: UniqFM Id Int
      unitIdExports = listToUFM $
                      concatMap (\(u,n) -> map (,n) (luIdExports u)) blocks

      -- OtherSymb -> Block
      unitOtherExports :: Map OtherSymb Int
      unitOtherExports = M.fromList $
                         concatMap (\(u,n) -> map (,n)
                                                  (map (OtherSymb mod)
                                                       (luOtherExports u)))
                                   blocks

      blocks :: [(LinkableUnit, Int)]
      blocks = zip units [0..]

      -- generate the list of exports and set of dependencies for one unit
      oneDep :: LinkableUnit
             -> Int
             -> StateT DependencyDataCache G (Int, BlockDeps, Bool, [ExportedFun])
      oneDep (LinkableUnit _ idExports otherExports idDeps pseudoIdDeps otherDeps req _frefs) n = do
        (edi, bdi) <- partitionEithers <$> mapM (lookupIdFun n) idDeps
        (edo, bdo) <- partitionEithers <$> mapM lookupOtherFun otherDeps
        (edp, bdp) <- partitionEithers <$> mapM (lookupPseudoIdFun n) pseudoIdDeps
        expi <- mapM lookupExportedId (filter isExportedId idExports)
        expo <- mapM lookupExportedOther otherExports
        -- fixme thin deps, remove all transitive dependencies!
        let bdeps = BlockDeps
                      { blockBlockDeps = IS.toList . IS.fromList . filter (/=n) $ bdi++bdo++bdp
                      , blockFunDeps   = S.toList . S.fromList $ edi++edo++edp
                      }
        return (n, bdeps, req, expi++expo)

      idModule :: Id -> Maybe Module
      idModule i = nameModule_maybe (getName i) >>= \m ->
                   guard (m /= mod) >> return m

      lookupPseudoIdFun :: Int -> Unique
                        -> StateT DependencyDataCache G (Either ExportedFun Int)
      lookupPseudoIdFun _n u =
        case lookupUFM_Directly unitIdExports u of
          Just k -> return (Right k)
          _      -> panic "lookupPseudoIdFun"

      -- get the function for an Id from the cache, add it if necessary
      -- result: Left Object.ExportedFun   if function refers to another module
      --         Right blockNumber if function refers to current module
      --
      --         assumes function is internal to the current block if it's
      --         from teh current module and not in the unitIdExports map.
      lookupIdFun :: Int -> Id
                  -> StateT DependencyDataCache G (Either ExportedFun Int)
      lookupIdFun n i = case lookupUFM unitIdExports i of
        Just k  -> return (Right k)
        Nothing -> case idModule i of
          Nothing -> return (Right n)
          Just m ->
            let k = getKey . getUnique $ i
                addEntry :: StateT DependencyDataCache G ExportedFun
                addEntry = do
                  (TxtI idTxt) <- lift (identForId i)
                  lookupExternalFun (Just k) (OtherSymb m idTxt)
            in  if m == mod
                   then pprPanic "local id not found" (ppr m)
                    else Left <$> do
                            mr <- gets (IM.lookup k . ddcId)
                            maybe addEntry return mr

      -- get the function for an OtherSymb from the cache, add it if necessary
      lookupOtherFun :: OtherSymb
                     -> StateT DependencyDataCache G (Either ExportedFun Int)
      lookupOtherFun od@(OtherSymb m idTxt) =
        case M.lookup od unitOtherExports of
          Just n  -> return (Right n)
          Nothing | m == mod -> pprPanic "genDependencyData.lookupOtherFun: unknown local other id:" (ftext idTxt)
          Nothing ->  Left <$> (maybe (lookupExternalFun Nothing od) return =<<
                        gets (M.lookup od . ddcOther))

      lookupExportedId :: Id -> StateT DependencyDataCache G ExportedFun
      lookupExportedId i = do
        (TxtI idTxt) <- lift (identForId i)
        lookupExternalFun (Just . getKey . getUnique $ i) (OtherSymb mod idTxt)

      lookupExportedOther :: FastString -> StateT DependencyDataCache G ExportedFun
      lookupExportedOther = lookupExternalFun Nothing . OtherSymb mod

      -- lookup a dependency to another module, add to the id cache if there's
      -- an id key, otherwise add to other cache
      lookupExternalFun :: Maybe Int
                        -> OtherSymb -> StateT DependencyDataCache G ExportedFun
      lookupExternalFun mbIdKey od@(OtherSymb m idTxt) = do
        let mk        = getKey . getUnique $ m
            mpk       = moduleUnit m
            exp_fun   = ExportedFun m (LexicalFastString idTxt)
            addCache  = do
              ms <- gets ddcModule
              let !cache' = IM.insert mk mpk ms
              modify (\s -> s { ddcModule = cache'})
              pure exp_fun
        f <- do
          mbm <- gets (IM.member mk . ddcModule)
          case mbm of
            False -> addCache
            True  -> pure exp_fun

        case mbIdKey of
          Nothing -> modify (\s -> s { ddcOther = M.insert od f (ddcOther s) })
          Just k  -> modify (\s -> s { ddcId    = IM.insert k f (ddcId s) })

        return f
