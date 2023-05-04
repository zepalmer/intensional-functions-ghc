{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal.Parse
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Extracting Haskell package metadata stored in Cabal files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal.Parse (
    parsePackageData, resolveContextData, parseCabalPkgId, configurePackage,
    buildAutogenFiles, copyPackage, writeInplacePkgConf, registerPackage
    ) where

import Data.Bifunctor
import Data.List.Extra
import Development.Shake
import qualified Distribution.Compat.Graph                     as Graph
import qualified Distribution.ModuleName                       as C
import qualified Distribution.Package                          as C
import qualified Distribution.PackageDescription               as C
import qualified Distribution.PackageDescription.Configuration as C
#if MIN_VERSION_Cabal(3,8,0)
import qualified Distribution.Simple.PackageDescription        as C
#else
import qualified Distribution.PackageDescription.Parsec        as C
#endif
import qualified Distribution.Simple.Compiler                  as C
import qualified Distribution.Simple.Program.Db                as C
import qualified Distribution.Simple                           as C
import qualified Distribution.Simple.Program.Builtin           as C
import qualified Distribution.Simple.Utils                     as C
import qualified Distribution.Simple.Program.Types             as C
import qualified Distribution.Simple.Configure                 as C (getPersistBuildConfig)
import qualified Distribution.Simple.Build                     as C
import qualified Distribution.Types.ComponentLocalBuildInfo    as C
import qualified Distribution.InstalledPackageInfo             as Installed
import qualified Distribution.Simple.PackageIndex              as C
import qualified Distribution.Text                             as C
import qualified Distribution.Types.LocalBuildInfo             as C
import qualified Distribution.Types.MungedPackageId            as C
#if MIN_VERSION_Cabal(3,5,0)
import qualified Distribution.Utils.Path                       as C
#endif
import qualified Distribution.Utils.ShortText                  as C
#if !MIN_VERSION_Cabal(3,4,0)
import qualified Distribution.Types.CondTree                   as C
#endif
#if !MIN_VERSION_Cabal(3,5,0)
import qualified Distribution.Types.ModuleReexport             as C
#endif
import qualified Distribution.Verbosity                        as C
import Hadrian.Expression
import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type
import Hadrian.Oracles.Cabal
import Hadrian.Oracles.ArgsHash
import Hadrian.Target

import Base
import Builder
import Context
import Flavour
import Settings
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.Simple.Register as C
import System.Directory (getCurrentDirectory)
import qualified Distribution.InstalledPackageInfo as CP
import Distribution.Simple.Utils (writeUTF8File)
import Utilities


-- | Parse the Cabal file of a given 'Package'. This operation is cached by the
-- "Hadrian.Oracles.TextFile.readPackageData" oracle.
parsePackageData :: Package -> Action PackageData
parsePackageData pkg = do
    gpd <- traced "cabal-read" $
        C.readGenericPackageDescription C.verbose (pkgCabalFile pkg)
    let pd      = C.packageDescription gpd
        pkgId   = C.package pd
        name    = C.unPackageName (C.pkgName pkgId)
        version = C.display (C.pkgVersion pkgId)
        libDeps = collectDeps (C.condLibrary gpd)
        exeDeps = map (collectDeps . Just . snd) (C.condExecutables gpd)
        allDeps = concat (libDeps : exeDeps)
        sorted  = sort [ C.unPackageName p | C.Dependency p _ _ <- allDeps ]
        deps    = nubOrd sorted \\ [name]
        depPkgs = mapMaybe findPackageByName deps
    return $ PackageData name version
                         (C.fromShortText (C.synopsis pd))
                         (C.fromShortText (C.description pd))
                         depPkgs gpd
  where
    -- Collect an overapproximation of dependencies by ignoring conditionals
    collectDeps :: Maybe (C.CondTree v [C.Dependency] a) -> [C.Dependency]
    collectDeps Nothing = []
    collectDeps (Just (C.CondNode _ deps ifs)) = deps ++ concatMap f ifs
      where
        f (C.CondBranch _ t mt) = collectDeps (Just t) ++ collectDeps mt

-- | Parse the package identifier from a Cabal file.
parseCabalPkgId :: FilePath -> IO String
parseCabalPkgId file = C.display . C.package . C.packageDescription <$> C.readGenericPackageDescription C.silent file

biModules :: C.PackageDescription -> (C.BuildInfo, [C.ModuleName], Maybe [C.ModuleName], Maybe (C.ModuleName, String))
biModules pd = go [ comp | comp@(bi,_,_,_) <-
                             (map libBiModules . maybeToList $ C.library pd) ++
                             (map exeBiModules               $ C.executables pd)
                         , C.buildable bi ]
  where
    libBiModules lib = (C.libBuildInfo lib, C.explicitLibModules lib, Just (map C.moduleReexportName (C.reexportedModules lib)),  Nothing)
    exeBiModules exe = (C.buildInfo exe,
                       -- If "main-is: ..." is not a .hs or .lhs file, do not
                       -- inject "Main" into the modules.  This does not respect
                       -- "-main-is" ghc-arguments! See Cabal's
                       -- Distribution.Simple.GHC for the glory details.
                       if takeExtension (C.modulePath exe) `elem` [".hs", ".lhs"]
                           then C.main : C.exeModules exe
                                -- The module `Main` still need to be kept in `modules` of PD.
                           else C.exeModules exe, Nothing,
                       Just (C.main, C.modulePath exe))
    go []  = error "No buildable component found."
    go [x] = x
    go _   = error "Cannot handle more than one buildinfo yet."

-- TODO: Track command line arguments and package configuration flags.
-- | Configure a package using the Cabal library by collecting all the command
-- line arguments (to be passed to the setup script) and package configuration
-- flags. The function 'need's package database entries for the dependencies of
-- the package the 'Context' points to.
configurePackage :: Context -> Action ()
configurePackage context@Context {..} = do
    putProgressInfo $ "| Configure package " ++ quote (pkgName package)
    gpd     <- pkgGenericDescription package
    depPkgs <- packageDependencies <$> readPackageData package

    -- Stage packages are those we have in this stage.
    stagePkgs <- stagePackages stage
    -- We'll need those packages in our package database.
    deps <- sequence [ pkgConfFile (context { package = pkg })
                     | pkg <- depPkgs, pkg `elem` stagePkgs ]
    need deps

    -- Figure out what hooks we need.
    hooks <- case C.buildType (C.flattenPackageDescription gpd) of
        C.Configure -> pure C.autoconfUserHooks
        C.Simple -> pure C.simpleUserHooks
        C.Make -> fail "build-type: Make is not supported"
        -- The 'time' package has a 'C.Custom' Setup.hs, but it's actually
        -- 'C.Configure' plus a @./Setup test@ hook. However, Cabal is also
        -- 'C.Custom', but doesn't have a configure script.
        C.Custom -> do
            configureExists <- doesFileExist $
                replaceFileName (pkgCabalFile package) "configure"
            pure $ if configureExists then C.autoconfUserHooks else C.simpleUserHooks

    -- Compute the list of flags, and the Cabal configuration arguments
    flagList    <- interpret (target context (Cabal Flags stage) [] []) getArgs
    argList     <- interpret (target context (Cabal Setup stage) [] []) getArgs
    trackArgsHash (target context (Cabal Flags stage) [] [])
    trackArgsHash (target context (Cabal Setup stage) [] [])
    verbosity   <- getVerbosity
    let v = if verbosity >= Diagnostic then "-v3" else "-v0"
        argList' = argList ++ ["--flags=" ++ unwords flagList, v]
    when (verbosity >= Verbose) $
        putProgressInfo $ "| Package " ++ quote (pkgName package) ++ " configuration flags: " ++ unwords argList'
    traced "cabal-configure" $
        C.defaultMainWithHooksNoReadArgs hooks gpd argList'

    dir <- Context.buildPath context
    files <- liftIO $ getDirectoryFilesIO "." [ dir -/- "include" -/- "**"
                                              , dir -/- "*.buildinfo"
                                              , dir -/- "lib" -/- "**"
                                              , dir -/- "config.*" ]
    produces files

-- | Copy the 'Package' of a given 'Context' into the package database
-- corresponding to the 'Stage' of the 'Context'.
copyPackage :: Context -> Action ()
copyPackage context@Context {..} = do
    putProgressInfo $ "| Copy package " ++ quote (pkgName package)
    gpd <- pkgGenericDescription package
    ctxPath   <- Context.contextPath context
    pkgDbPath <- packageDbPath (PackageDbLoc stage iplace)
    verbosity <- getVerbosity
    let v = if verbosity >= Diagnostic then "-v3" else "-v0"
    traced "cabal-copy" $
        C.defaultMainWithHooksNoReadArgs C.autoconfUserHooks gpd
            [ "copy", "--builddir", ctxPath, "--target-package-db", pkgDbPath, v ]



-- | What type of file is Main
data MainSourceType = HsMain | CppMain | CMain

-- | Parse the 'ContextData' of a given 'Context'.
resolveContextData :: Context -> Action ContextData
resolveContextData context@Context {..} = do
    cPath <- Context.contextPath context
    lbi <- liftIO $ C.getPersistBuildConfig cPath

    -- Note: the @cPath@ is ignored. The path that's used is the 'buildDir' path
    -- from the local build info @lbi@.
    pdi <- liftIO $ getHookedBuildInfo [pkgPath package, cPath -/- "build"]
    let pd'  = C.updatePackageDescription pdi (C.localPkgDescr lbi)
        lbi' = lbi { C.localPkgDescr = pd' }

    -- TODO: Get rid of deprecated 'externalPackageDeps' and drop -Wno-deprecations
    -- See: https://github.com/snowleopard/hadrian/issues/548
    let extDeps   = externalPackageDeps lbi'
        deps      = map (C.display . snd) extDeps
        depDirect = map (fromMaybe (error "resolveContextData: depDirect failed")
                  . C.lookupUnitId (C.installedPkgs lbi') . fst) extDeps
        depIds    = map (C.display . Installed.installedUnitId) depDirect
        ghcProg   =
          case C.lookupProgram C.ghcProgram (C.withPrograms lbi') of
            Just ghc -> ghc
            Nothing  -> error "resolveContextData: failed to look up 'ghc'"

        depPkgs   = C.topologicalOrder (packageHacks (C.installedPkgs lbi'))
        forDeps f = concatMap f depPkgs

        -- Copied from Distribution.Simple.PreProcess.ppHsc2Hs
        packageHacks = case C.compilerFlavor (C.compiler lbi') of
            C.GHC | C.pkgName (C.package pd') /= (C.mkPackageName "rts") -> hackRtsPackage
            _   -> id

        -- TODO: Get rid of this hack.
        -- We don't link in the actual Haskell libraries of our dependencies, so
        -- the "-u" flags in @ldOptions@ of the @rts@ package mean linking fails
        -- on OS X (its @ld@ is a tad stricter than GNU @ld@). Thus we remove
        -- @ldOptions@ for the @rts@ package. With one exception (see below).
        hackRtsPackage index | null (C.allPackages index) = index
        -- ^ do not hack the empty index
        hackRtsPackage index = case C.lookupPackageName index (C.mkPackageName "rts") of
            [(_, [rts])] -> C.insert rts {
                Installed.ldOptions   = [],
                Installed.libraryDirs = filter (not . ("gcc-lib" `isSuffixOf`))
                                               (Installed.libraryDirs rts)} index
            -- GHC <= 6.12 had @$topdir/gcc-lib@ in their @library-dirs@ for the
            -- 'rts' package, which causes problems when we try to use the
            -- in-tree @mingw@, due to accidentally picking up the incompatible
            -- libraries there. So we filter out @gcc-lib@ from the RTS's
            -- @library-dirs@ here.
            _ -> error "No (or multiple) GHC rts package is registered!"

        (buildInfo, modules, rexport_modules, mainIs) = biModules (C.localPkgDescr lbi')

        classifyMain :: FilePath -> MainSourceType
        classifyMain fp
          | takeExtension fp `elem` [".hs", ".lhs"] = HsMain
          | takeExtension fp `elem` [".cpp", ".cxx", ".c++"]= CppMain
          | otherwise = CMain

        main_src = fmap (first C.display) mainIs
        cdata = ContextData
          { dependencies    = deps
          , componentId     = C.localCompatPackageKey lbi'
          , mainIs          = main_src
          , modules         = map C.display modules
          , otherModules    = map C.display $ C.otherModules buildInfo
          , reexportModules = map C.display (concat rexport_modules)
          , srcDirs         =
#if MIN_VERSION_Cabal(3,5,0)
                              map C.getSymbolicPath
#endif
                                  (C.hsSourceDirs buildInfo)
          , depIds          = depIds
          , depNames        = map (C.display . C.mungedName . snd) extDeps
          , includeDirs     = C.includeDirs     buildInfo
          , includes        = C.includes        buildInfo
          , installIncludes = C.installIncludes buildInfo
          , extraLibs       = C.extraLibs       buildInfo
          , extraLibDirs    = C.extraLibDirs    buildInfo
          , asmSrcs         = C.asmSources      buildInfo
          , cSrcs           = C.cSources        buildInfo ++ [ ms | Just (_,ms) <- pure main_src, CMain   <- pure (classifyMain ms)]
          , cxxSrcs         = C.cxxSources      buildInfo ++ [ ms | Just (_,ms) <- pure main_src, CppMain <- pure (classifyMain ms)]
          , cmmSrcs         = C.cmmSources      buildInfo
          , jsSrcs          = C.jsSources       buildInfo
          , hcOpts          = C.programDefaultArgs ghcProg
              ++ C.hcOptions C.GHC buildInfo
              ++ C.languageToFlags   (C.compiler lbi') (C.defaultLanguage buildInfo)
              ++ C.extensionsToFlags (C.compiler lbi') (C.usedExtensions  buildInfo)
              ++ C.programOverrideArgs ghcProg
          , asmOpts            = C.asmOptions buildInfo
          , ccOpts             = C.ccOptions  buildInfo
          , cxxOpts            = C.cxxOptions buildInfo
          , cmmOpts            = C.cmmOptions buildInfo
          , cppOpts            = C.cppOptions buildInfo
          , ldOpts             = C.ldOptions  buildInfo
          , depIncludeDirs     = forDeps Installed.includeDirs
          , depCcOpts          = forDeps Installed.ccOptions
          , depLdOpts          = forDeps Installed.ldOptions
          , buildGhciLib       = C.withGHCiLib lbi'
          , frameworks         = C.frameworks buildInfo
          , packageDescription = pd' }

      in return cdata

-- Writes a .conf file which points directly into the build directory of a package
-- so the artefacts can be used as they are produced.
write_inplace_conf :: FilePath -> FilePath -> C.PackageDescription -> LocalBuildInfo -> IO ()
write_inplace_conf pkg_path res_path pd lbi = do
       withLibLBI pd lbi $ \lib clbi ->
           do cwd <- getCurrentDirectory
              let fixupIncludeDir dir | cwd `isPrefixOf` dir = [prefix ++ drop (length cwd) dir]
                                      | otherwise            = [dir]
                    where
                      prefix = "${pkgroot}/../../../"
              let installedPkgInfo =

                    C.inplaceInstalledPackageInfo (cwd </> pkg_path) build_dir pd (C.mkAbiHash "inplace") lib lbi clbi

                  build_dir = "${pkgroot}/../" ++ pkg_path ++ "/build"
                  pkg_name = C.display (C.pkgName (CP.sourcePackageId installedPkgInfo))
                  final_ipi = installedPkgInfo {
                                 Installed.includeDirs = concatMap fixupIncludeDir (Installed.includeDirs installedPkgInfo),
                                 Installed.libraryDirs = build_dir: (concatMap fixupIncludeDir (Installed.libraryDirs installedPkgInfo)) ,
#if MIN_VERSION_Cabal(3,8,0)
                                 Installed.libraryDirsStatic = build_dir: (concatMap fixupIncludeDir (Installed.libraryDirsStatic installedPkgInfo)) ,
#endif
                                 Installed.libraryDynDirs = build_dir : (concatMap fixupIncludeDir (Installed.libraryDynDirs installedPkgInfo)) ,
                                 Installed.dataDir = "${pkgroot}/../../../../" ++ pkg_path,
                                 Installed.haddockHTMLs = [build_dir ++ "/doc/html/" ++ C.display (CP.sourcePackageId installedPkgInfo)],
                                 Installed.haddockInterfaces = [build_dir ++ "/doc/html/" ++  pkg_name ++ "/" ++ pkg_name ++ ".haddock"],
                                 Installed.importDirs = [build_dir]

                              }

                  content = Installed.showInstalledPackageInfo final_ipi ++ "\n"
              C.writeFileAtomic res_path
                              (C.toUTF8LBS content)

-- This uses the API directly because no way to register into a different package db which is
-- configured. See the use of C.SpecificPackageDB
registerPackage :: [(Resource, Int)] -> Context -> Action ()
registerPackage rs context = do
    cPath <- Context.contextPath context
    setupConfig <- pkgSetupConfigFile context
    need [setupConfig] -- This triggers 'configurePackage'
    pd <- packageDescription <$> readContextData context
    db_path <- packageDbPath (PackageDbLoc (stage context) (iplace context))
    dist_dir <- Context.buildPath context
    pid <- pkgIdentifier (package context)
    -- Note: the @cPath@ is ignored. The path that's used is the 'buildDir' path
    -- from the local build info @lbi@.
    lbi <- liftIO $ C.getPersistBuildConfig cPath
    liftIO $ register db_path pid dist_dir pd lbi
    -- Then after the register, which just writes the .conf file, do the recache step.
    buildWithResources rs $
      target context (GhcPkg Recache (stage context)) [] []

-- This is copied and simplified from Cabal, because we want to install the package
-- into a different package database to the one it was configured against.
register :: FilePath
         -> FilePath
         -> FilePath
         -> C.PackageDescription
         -> LocalBuildInfo
         -> IO ()
register pkg_db conf_file build_dir pd lbi
  = withLibLBI pd lbi $ \lib clbi -> do

    absPackageDBs    <- C.absolutePackageDBPaths packageDbs
    installedPkgInfo <- C.generateRegistrationInfo
                           C.silent pd lib lbi clbi False reloc build_dir
                           (C.registrationPackageDB absPackageDBs)

    writeRegistrationFile installedPkgInfo

  where
    regFile             = conf_file
    reloc     = relocatable lbi
    -- Using a specific package db here is why we have to copy the function from Cabal.
    packageDbs = [C.SpecificPackageDB pkg_db]

    writeRegistrationFile installedPkgInfo = do
      writeUTF8File (pkg_db </> regFile <.> "conf") (CP.showInstalledPackageInfo installedPkgInfo)


-- | Build autogenerated files @autogen/cabal_macros.h@ and @autogen/Paths_*.hs@.
buildAutogenFiles :: Context -> Action ()
buildAutogenFiles context = do
    cPath <- Context.contextPath context
    setupConfig <- pkgSetupConfigFile context
    need [setupConfig] -- This triggers 'configurePackage'
    pd <- packageDescription <$> readContextData context
    -- Note: the @cPath@ is ignored. The path that's used is the 'buildDir' path
    -- from the local build info @lbi@.
    traced "cabal-autogen" $ do
        lbi <- C.getPersistBuildConfig cPath
        C.initialBuildSteps cPath pd (lbi { C.localPkgDescr = pd }) C.silent

-- | Write a .conf file for the inplace package database which points into the
-- build directories rather than the final install locations.
writeInplacePkgConf :: Context -> Action ()
writeInplacePkgConf context = do
    cPath <- Context.contextPath context
    setupConfig <- pkgSetupConfigFile context
    need [setupConfig] -- This triggers 'configurePackage'
    pd <- packageDescription <$> readContextData context
    conf <- pkgInplaceConfig context
    -- Note: the @cPath@ is ignored. The path that's used is the 'buildDir' path
    -- from the local build info @lbi@.
    lbi <- liftIO $ C.getPersistBuildConfig cPath
    liftIO $ write_inplace_conf (pkgPath (package context)) conf pd (lbi { C.localPkgDescr = pd })


-- | Look for a @.buildinfo@ in all of the specified directories, stopping on
-- the first one we find.
getHookedBuildInfo :: [FilePath] -> IO C.HookedBuildInfo
getHookedBuildInfo [] = return C.emptyHookedBuildInfo
getHookedBuildInfo (baseDir:baseDirs) = do
    maybeInfoFile <- C.findHookedPackageDesc C.normal baseDir
    case maybeInfoFile of
        Nothing       -> getHookedBuildInfo baseDirs
        Just infoFile -> C.readHookedBuildInfo C.silent infoFile

externalPackageDeps :: C.LocalBuildInfo -> [(C.UnitId, C.MungedPackageId)]
externalPackageDeps lbi =
    -- TODO:  what about non-buildable components?
    nub [ (ipkgid, pkgid)
        | clbi            <- Graph.toList (C.componentGraph lbi)
        , (ipkgid, pkgid) <- C.componentPackageDeps clbi
        , not (internal ipkgid) ]
  where
    -- True if this dependency is an internal one (depends on the library
    -- defined in the same package).
    internal ipkgid = any ((==ipkgid) . C.componentUnitId) (Graph.toList (C.componentGraph lbi))

