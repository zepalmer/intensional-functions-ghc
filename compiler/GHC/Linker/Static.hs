module GHC.Linker.Static
   ( linkBinary
   , linkBinary'
   , linkStaticLib
   , exeFileName
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Platform.Ways
import GHC.Settings

import GHC.SysTools
import GHC.SysTools.Ar

import GHC.Unit.Env
import GHC.Unit.Types
import GHC.Unit.Info
import GHC.Unit.State

import GHC.Utils.Logger
import GHC.Utils.Monad
import GHC.Utils.Misc
import GHC.Utils.TmpFs

import GHC.Linker.MacOS
import GHC.Linker.Unit
import GHC.Linker.Dynamic
import GHC.Linker.ExtraObj
import GHC.Linker.Windows

import GHC.Driver.Session

import System.FilePath
import System.Directory
import Control.Monad
import Data.Maybe

-----------------------------------------------------------------------------
-- Static linking, of .o files

-- The list of packages passed to link is the list of packages on
-- which this program depends, as discovered by the compilation
-- manager.  It is combined with the list of packages that the user
-- specifies on the command line with -package flags.
--
-- In one-shot linking mode, we can't discover the package
-- dependencies (because we haven't actually done any compilation or
-- read any interface files), so the user must explicitly specify all
-- the packages.

{-
Note [-Xlinker -rpath vs -Wl,-rpath]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-Wl takes a comma-separated list of options which in the case of
-Wl,-rpath -Wl,some,path,with,commas parses the path with commas
as separate options.
Buck, the build system, produces paths with commas in them.

-Xlinker doesn't have this disadvantage and as far as I can tell
it is supported by both gcc and clang. Anecdotally nvcc supports
-Xlinker, but not -Wl.
-}

linkBinary :: Logger -> TmpFs -> DynFlags -> UnitEnv -> [FilePath] -> [UnitId] -> IO ()
linkBinary = linkBinary' False

linkBinary' :: Bool -> Logger -> TmpFs -> DynFlags -> UnitEnv -> [FilePath] -> [UnitId] -> IO ()
linkBinary' staticLink logger tmpfs dflags unit_env o_files dep_units = do
    let platform   = ue_platform unit_env
        unit_state = ue_units unit_env
        toolSettings' = toolSettings dflags
        verbFlags = getVerbFlags dflags
        output_fn = exeFileName platform staticLink (outputFile dflags)

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)
    pkgs <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_units)
    let pkg_lib_paths     = collectLibraryDirs (ways dflags) pkgs
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                  rpath = if useXLinkerRPath dflags (platformOS platform)
                          then ["-Xlinker", "-rpath", "-Xlinker", libpath]
                          else []
                  -- Solaris 11's linker does not support -rpath-link option. It silently
                  -- ignores it and then complains about next option which is -l<some
                  -- dir> as being a directory and not expected object file, E.g
                  -- ld: elf error: file
                  -- /tmp/ghc-src/libraries/base/dist-install/build:
                  -- elf_begin: I/O error: region read: Is a directory
                  rpathlink = if (platformOS platform) == OSSolaris2
                              then []
                              else ["-Xlinker", "-rpath-link", "-Xlinker", l]
              in ["-L" ++ l] ++ rpathlink ++ rpath
         | osMachOTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags &&
           useXLinkerRPath dflags (platformOS platform)
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "@loader_path" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
              in ["-L" ++ l] ++ ["-Xlinker", "-rpath", "-Xlinker", libpath]
         | otherwise = ["-L" ++ l]

    pkg_lib_path_opts <-
      if gopt Opt_SingleLibFolder dflags
      then do
        libs <- getLibs dflags unit_env dep_units
        tmpDir <- newTempDir logger tmpfs dflags
        sequence_ [ copyFile lib (tmpDir </> basename)
                  | (lib, basename) <- libs]
        return [ "-L" ++ tmpDir ]
      else pure pkg_lib_path_opts

    let
      dead_strip
        | gopt Opt_WholeArchiveHsLibs dflags = []
        | otherwise = if osSubsectionsViaSymbols (platformOS platform)
                        then ["-Wl,-dead_strip"]
                        else []
    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- maybeToList <$> mkExtraObjToLinkIntoBinary logger tmpfs dflags unit_state
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary logger tmpfs dflags unit_env dep_units

    let
      (pre_hs_libs, post_hs_libs)
        | gopt Opt_WholeArchiveHsLibs dflags
        = if platformOS platform == OSDarwin
            then (["-Wl,-all_load"], [])
              -- OS X does not have a flag to turn off -all_load
            else (["-Wl,--whole-archive"], ["-Wl,--no-whole-archive"])
        | otherwise
        = ([],[])

    pkg_link_opts <- do
        (package_hs_libs, extra_libs, other_flags) <- getUnitLinkOpts dflags unit_env dep_units
        return $ if staticLink
            then package_hs_libs -- If building an executable really means making a static
                                 -- library (e.g. iOS), then we only keep the -l options for
                                 -- HS packages, because libtool doesn't accept other options.
                                 -- In the case of iOS these need to be added by hand to the
                                 -- final link in Xcode.
            else other_flags ++ dead_strip
                  ++ pre_hs_libs ++ package_hs_libs ++ post_hs_libs
                  ++ extra_libs
                 -- -Wl,-u,<sym> contained in other_flags
                 -- needs to be put before -l<package>,
                 -- otherwise Solaris linker fails linking
                 -- a binary with unresolved symbols in RTS
                 -- which are defined in base package
                 -- the reason for this is a note in ld(1) about
                 -- '-u' option: "The placement of this option
                 -- on the command line is significant.
                 -- This option must be placed before the library
                 -- that defines the symbol."

    -- frameworks
    pkg_framework_opts <- getUnitFrameworkOpts unit_env dep_units
    let framework_opts = getFrameworkOpts dflags platform

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    rc_objs <- case platformOS platform of
      OSMinGW32 | gopt Opt_GenManifest dflags -> maybeCreateManifest logger tmpfs dflags output_fn
      _                                       -> return []

    let link dflags args | staticLink = GHC.SysTools.runLibtool logger dflags args
                         | platformOS platform == OSDarwin
                            = do
                                 GHC.SysTools.runLink logger tmpfs dflags args
                                 GHC.Linker.MacOS.runInjectRPaths logger dflags pkg_lib_paths output_fn
                         | otherwise
                            = GHC.SysTools.runLink logger tmpfs dflags args

    link dflags (
                       map GHC.SysTools.Option verbFlags
                      ++ [ GHC.SysTools.Option "-o"
                         , GHC.SysTools.FileOption "" output_fn
                         ]
                      ++ libmLinkOpts
                      ++ map GHC.SysTools.Option (
                         []

                      -- See Note [No PIE when linking]
                      ++ picCCOpts dflags

                      -- Permit the linker to auto link _symbol to _imp_symbol.
                      -- This lets us link against DLLs without needing an "import library".
                      ++ (if platformOS platform == OSMinGW32
                          then ["-Wl,--enable-auto-import"]
                          else [])

                      -- '-no_compact_unwind'
                      -- C++/Objective-C exceptions cannot use optimised
                      -- stack unwinding code. The optimised form is the
                      -- default in Xcode 4 on at least x86_64, and
                      -- without this flag we're also seeing warnings
                      -- like
                      --     ld: warning: could not create compact unwind for .LFB3: non-standard register 5 being saved in prolog
                      -- on x86.
                      ++ (if not (gopt Opt_CompactUnwind dflags) &&
                             toolSettings_ldSupportsCompactUnwind toolSettings' &&
                             not staticLink &&
                             (platformOS platform == OSDarwin) &&
                             case platformArch platform of
                               ArchX86     -> True
                               ArchX86_64  -> True
                               ArchARM {}  -> True
                               ArchAArch64 -> True
                               _ -> False
                          then ["-Wl,-no_compact_unwind"]
                          else [])

                      -- '-Wl,-read_only_relocs,suppress'
                      -- ld gives loads of warnings like:
                      --     ld: warning: text reloc in _base_GHCziArr_unsafeArray_info to _base_GHCziArr_unsafeArray_closure
                      -- when linking any program. We're not sure
                      -- whether this is something we ought to fix, but
                      -- for now this flags silences them.
                      ++ (if platformOS   platform == OSDarwin &&
                             platformArch platform == ArchX86 &&
                             not staticLink
                          then ["-Wl,-read_only_relocs,suppress"]
                          else [])

                      ++ (if toolSettings_ldIsGnuLd toolSettings' &&
                             not (gopt Opt_WholeArchiveHsLibs dflags)
                          then ["-Wl,--gc-sections"]
                          else [])

                      ++ o_files
                      ++ lib_path_opts)
                      ++ extra_ld_inputs
                      ++ map GHC.SysTools.Option (
                         rc_objs
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj
                      ++ noteLinkObjs
                      ++ pkg_link_opts
                      ++ pkg_framework_opts
                      ++ (if platformOS platform == OSDarwin
                          --  dead_strip_dylibs, will remove unused dylibs, and thus save
                          --  space in the load commands. The -headerpad is necessary so
                          --  that we can inject more @rpath's later for the left over
                          --  libraries during runInjectRpaths phase.
                          --
                          --  See Note [Dynamic linking on macOS].
                          then [ "-Wl,-dead_strip_dylibs", "-Wl,-headerpad,8000" ]
                          else [])
                    ))

-- | Linking a static lib will not really link anything. It will merely produce
-- a static archive of all dependent static libraries. The resulting library
-- will still need to be linked with any remaining link flags.
linkStaticLib :: Logger -> DynFlags -> UnitEnv -> [String] -> [UnitId] -> IO ()
linkStaticLib logger dflags unit_env o_files dep_units = do
  let platform  = ue_platform unit_env
      extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
      modules = o_files ++ extra_ld_inputs
      output_fn = exeFileName platform True (outputFile dflags)

  full_output_fn <- if isAbsolute output_fn
                    then return output_fn
                    else do d <- getCurrentDirectory
                            return $ normalise (d </> output_fn)
  output_exists <- doesFileExist full_output_fn
  (when output_exists) $ removeFile full_output_fn

  pkg_cfgs_init <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_units)

  let pkg_cfgs
        | gopt Opt_LinkRts dflags
        = pkg_cfgs_init
        | otherwise
        = filter ((/= rtsUnitId) . unitId) pkg_cfgs_init

  archives <- concatMapM (collectArchives dflags) pkg_cfgs

  ar <- foldl mappend
        <$> (Archive <$> mapM loadObj modules)
        <*> mapM loadAr archives

  if toolSettings_ldIsGnuLd (toolSettings dflags)
    then writeGNUAr output_fn $ afilter (not . isGNUSymdef) ar
    else writeBSDAr output_fn $ afilter (not . isBSDSymdef) ar

  -- run ranlib over the archive. write*Ar does *not* create the symbol index.
  runRanlib logger dflags [GHC.SysTools.FileOption "" output_fn]



-- | Compute the output file name of a program.
--
-- StaticLink boolean is used to indicate if the program is actually a static library
-- (e.g., on iOS).
--
-- Use the provided filename (if any), otherwise use "main.exe" (Windows),
-- "a.out (otherwise without StaticLink set), "liba.a". In every case, add the
-- extension if it is missing.
exeFileName :: Platform -> Bool -> Maybe FilePath -> FilePath
exeFileName platform staticLink output_fn
  | Just s <- output_fn =
      case platformOS platform of
          OSMinGW32 -> s <?.> "exe"
          _         -> if staticLink
                         then s <?.> "a"
                         else s
  | otherwise =
      if platformOS platform == OSMinGW32
      then "main.exe"
      else if staticLink
           then "liba.a"
           else "a.out"
 where s <?.> ext | null (takeExtension s) = s <.> ext
                  | otherwise              = s
