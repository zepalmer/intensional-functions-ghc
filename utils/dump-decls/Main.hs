module Main where

import GHC
import GHC.Core.InstEnv (instEnvElts, instanceHead)
import GHC.Core.TyCo.FVs (tyConsOfType)
import GHC.Driver.Ppr (showSDocForUser)
import GHC.Unit.State (lookupUnitId, lookupPackageName)
import GHC.Unit.Info (UnitInfo, unitExposedModules, PackageName(..))
import GHC.Data.FastString (fsLit)
import GHC.Driver.Env (hsc_units, hscEPS)
import GHC.Utils.Outputable
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.TyThing.Ppr (pprTyThing)
import GHC.Types.Name (nameOccName, nameModule_maybe)
import GHC.Unit.External (eps_inst_env)
import GHC.Iface.Syntax (showToHeader)

import Data.Function (on)
import Data.List (sortBy)
import Control.Monad.IO.Class
import System.Environment (getArgs)
import Prelude hiding ((<>))

main :: IO ()
main = do
    ghcRoot:pkg_names <- getArgs
    mapM_ (run ghcRoot) pkg_names

run :: FilePath -> String -> IO ()
run root pkg_nm = runGhc (Just root) $ do
    let args = map noLoc ["-package=" ++ pkg_nm, "-dppr-cols=1000"]
    dflags <- do
        dflags <- getSessionDynFlags
        logger <- getLogger
        (dflags', _fileish_args, _dynamicFlagWarnings) <-
          GHC.parseDynamicFlags logger dflags args
        return dflags'

    _ <- setProgramDynFlags dflags
    unit_state <- hsc_units <$> getSession
    unit_id <- case lookupPackageName unit_state (PackageName $ fsLit pkg_nm) of
                    Just unit_id -> return unit_id
                    Nothing -> fail "failed to find package"
    unit_info <- case lookupUnitId unit_state unit_id of
      Just unit_info -> return unit_info
      Nothing -> fail "unknown package"

    decls_doc <- reportUnitDecls unit_info
    insts_doc <- reportInstances

    name_ppr_ctx <- GHC.getNamePprCtx
    let rendered = showSDocForUser dflags unit_state name_ppr_ctx (vcat [decls_doc, insts_doc])
    liftIO $ putStrLn rendered

ignoredModules :: [ModuleName]
ignoredModules =
    map mkModuleName $ concat
    [ unstableModules
    , platformDependentModules
    ]
  where
    unstableModules =
        [ "GHC.Prim"
        , "GHC.Conc.POSIX"
        , "GHC.Conc.IO"
        ]
    platformDependentModules =
        [ "System.Posix.Types"
        , "Foreign.C.Types"
        ]

ignoredName :: Name -> Bool
ignoredName nm
  | Just md <- nameModule_maybe nm
  , moduleName md `elem` ignoredModules
  = True
  | otherwise
  = False

ignoredTyThing :: TyThing -> Bool
ignoredTyThing _ = False

ignoredTyCon :: TyCon -> Bool
ignoredTyCon = ignoredName . getName

ignoredType :: Type -> Bool
ignoredType = any ignoredTyCon . nonDetEltsUniqSet . tyConsOfType

-- | Ignore instances whose heads mention ignored types.
ignoredInstance :: ClsInst -> Bool
ignoredInstance inst
  | ignoredName $ getName cls
  = True
  | any ignoredType tys
  = True
  | otherwise
  = False
  where
    (_, cls, tys) = instanceHead inst

reportUnitDecls :: UnitInfo -> Ghc SDoc
reportUnitDecls unit_info = do
    let exposed :: [ModuleName]
        exposed = map fst (unitExposedModules unit_info)
    vcat <$> mapM reportModuleDecls exposed

reportModuleDecls :: ModuleName -> Ghc SDoc
reportModuleDecls modl_nm
  | modl_nm `elem` ignoredModules = do
      return $ vcat [ mod_header, text "-- ignored", text "" ]
  | otherwise = do
    modl <- GHC.lookupQualifiedModule NoPkgQual modl_nm
    mb_mod_info <- GHC.getModuleInfo modl
    mod_info <- case mb_mod_info of
      Nothing -> fail "Failed to find module"
      Just mod_info -> return mod_info

    Just name_ppr_ctx <- mkNamePprCtxForModule mod_info
    let names = GHC.modInfoExports mod_info
        sorted_names = sortBy (compare `on` nameOccName) names
    things <- mapM GHC.lookupName sorted_names
    return $ withUserStyle name_ppr_ctx AllTheWay $
        hang mod_header 2
          (vcat
            [ pprTyThing showToHeader thing
            | Just thing <- things
            , not $ ignoredTyThing thing
            ]) <>
        text ""
  where
    mod_header = vcat
        [ text ""
        , text "module" <+> ppr modl_nm <+> text "where"
        , text ""
        ]

reportInstances :: Ghc SDoc
reportInstances = do
    hsc_env <- getSession
    eps <- liftIO $ hscEPS hsc_env
    let instances = eps_inst_env eps
    return $ vcat $
        [ text ""
        , text ""
        , text "-- Instances:"
        ] ++
        [ ppr inst
        | inst <- instEnvElts instances
        , not $ ignoredInstance inst
        ]
