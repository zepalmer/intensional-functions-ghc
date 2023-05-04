module Settings.Default (
    -- * Packages that are build by default and for the testsuite
    defaultPackages, testsuitePackages, stage0Packages,

    -- * Default build ways
    defaultLibraryWays, defaultRtsWays,

    -- * Default command line arguments for various builders
    SourceArgs (..), sourceArgs, defaultBuilderArgs, defaultPackageArgs,
    defaultExtraArgs,

    -- * Default build flavour and BigNum backend
    defaultFlavour, defaultBignumBackend
    ) where

import qualified Data.Set as Set

import qualified Hadrian.Builder.Sphinx
import qualified Hadrian.Builder.Tar

import CommandLine
import Expression
import Flavour.Type
import Oracles.Flag
import Oracles.Setting
import Packages
import Settings.Builders.Alex
import Settings.Builders.DeriveConstants
import Settings.Builders.Cabal
import Settings.Builders.Cc
import Settings.Builders.Configure
import Settings.Builders.GenPrimopCode
import Settings.Builders.Ghc
import Settings.Builders.GhcPkg
import Settings.Builders.Haddock
import Settings.Builders.Happy
import Settings.Builders.Hsc2Hs
import Settings.Builders.HsCpp
import Settings.Builders.Ar
import Settings.Builders.Ld
import Settings.Builders.Make
import Settings.Builders.MergeObjects
import Settings.Builders.SplitSections
import Settings.Builders.RunTest
import Settings.Builders.Xelatex
import Settings.Packages
import Settings.Warnings
import qualified Hadrian.Builder.Git
import Settings.Builders.Win32Tarballs

-- | Packages that are built by default. You can change this in "UserSettings".
defaultPackages :: Stage -> Action [Package]
defaultPackages (Stage0 GlobalLibs) = stageBootPackages
defaultPackages (Stage0 InTreeLibs) = stage0Packages
defaultPackages Stage1 = stage1Packages
defaultPackages Stage2 = stage2Packages
defaultPackages Stage3 = return []

-- | Default bignum backend.
defaultBignumBackend :: String
defaultBignumBackend = "gmp"

-- These packages are things needed to do the build.. so they are only built by
-- boot compiler, with global package database. By default we will only build these
-- packages in StageBoot so if you also need to distribute anything here then add
-- it to `stage0packages` or `stage1packages` as appropiate.
stageBootPackages :: Action [Package]
stageBootPackages = return [lintersCommon, lintCommitMsg, lintSubmoduleRefs, lintWhitespace, lintNotes, hsc2hs, compareSizes, deriveConstants, genapply, genprimopcode, unlit ]

-- | Packages built in 'Stage0' by default. You can change this in "UserSettings".
stage0Packages :: Action [Package]
stage0Packages = do
    cross <- flag CrossCompiling
    return $ [ binary
             , bytestring
             , cabalSyntax
             , cabal
             , compiler
             , containers
             , directory
             , process
             , exceptions
             , filepath
             , ghc
             , runGhc
             , ghcBoot
             , ghcBootTh
             , ghcHeap
             , ghci
             , ghcPkg
             , haddock
             , hsc2hs
             , hpc
             , hpcBin
             , mtl
             , parsec
             , semaphoreCompat
             , time
             , templateHaskell
             , text
             , transformers
             , unlit
             , hp2ps
             , if windowsHost then win32 else unix
             ]
          ++ [ terminfo | not windowsHost, not cross ]
          ++ [ timeout  | windowsHost                ]
          ++ [ touchy   | windowsHost                ]

-- | Packages built in 'Stage1' by default. You can change this in "UserSettings".
stage1Packages :: Action [Package]
stage1Packages = do
    let good_stage0_package p
          -- we only keep libraries for some reason
          | not (isLibrary p) = False
          -- but not win32/unix because it depends on cross-compilation target
          | p == win32        = False
          | p == unix         = False
          | otherwise         = True

    libraries0 <- filter good_stage0_package <$> stage0Packages
    cross      <- flag CrossCompiling
    winTarget  <- isWinTarget

    let when c xs = if c then xs else mempty

    return $ mconcat
      [ libraries0 -- Build all Stage0 libraries in Stage1
      , [ array
        , base
        , containers
        , deepseq
        , exceptions
        , ghc
        , ghcBignum
        , ghcCompact
        , ghcPkg
        , ghcPrim
        , haskeline
        , hp2ps
        , hsc2hs
        , integerGmp
        , pretty
        , rts
        , semaphoreCompat
        , stm
        , unlit
        , xhtml
        , if winTarget then win32 else unix
        ]
      , when (not cross)
        [ haddock
        , hpcBin
        , iserv
        , runGhc
        ]
      , when (winTarget && not cross)
        [ touchy
         -- See Note [Hadrian's ghci-wrapper package]
        , ghciWrapper
        ]
      ]

-- | Packages built in 'Stage2' by default. You can change this in "UserSettings".
stage2Packages :: Action [Package]
stage2Packages = stage1Packages

-- | Packages that are built only for the testsuite.
testsuitePackages :: Action [Package]
testsuitePackages = return ([ timeout | windowsHost ] ++ [ checkPpr, checkExact, countDeps, ghcConfig ])

-- | Default build ways for library packages:
-- * We always build 'vanilla' way.
-- * We build 'profiling' way when stage > Stage0.
-- * We build 'dynamic' way when stage > Stage0 and the platform supports it.
defaultLibraryWays :: Ways
defaultLibraryWays = Set.fromList <$>
    mconcat
    [ pure [vanilla]
    , notStage0 ? pure [profiling]
    , notStage0 ? platformSupportsSharedLibs ? pure [dynamic]
    ]

-- | Default build ways for the RTS.
defaultRtsWays :: Ways
defaultRtsWays = Set.fromList <$>
  mconcat
  [ pure [vanilla]
  , notStage0 ? pure
      [ profiling, debugProfiling
      , debug
      ]
  , notStage0 ? targetSupportsThreadedRts ? pure [threaded, threadedProfiling, threadedDebugProfiling, threadedDebug]
  , notStage0 ? platformSupportsSharedLibs ? pure
      [ dynamic, debugDynamic
      ]
  , notStage0 ? platformSupportsSharedLibs ? targetSupportsThreadedRts ? pure [ threadedDynamic, threadedDebugDynamic ]
  ]

-- TODO: Move C source arguments here
-- | Default and package-specific source arguments.
data SourceArgs = SourceArgs
    { hsDefault  :: Args
    , hsLibrary  :: Args
    , hsCompiler :: Args
    , hsGhc      :: Args }

-- | Concatenate source arguments in appropriate order.
sourceArgs :: SourceArgs -> Args
sourceArgs SourceArgs {..} = builder Ghc ? mconcat
    [ hsDefault
    -- `compiler` is also a library but the specific arguments that we want
    -- to apply to that are given by the hsCompiler option. `ghc` is an
    -- executable so we don't have to exclude that.
    , libraryPackage   ? notM (packageOneOf [compiler]) ? hsLibrary
    , package compiler ? hsCompiler
    , package ghc      ? hsGhc ]

-- | All default command line arguments.
defaultExtraArgs :: Args
defaultExtraArgs = sourceArgs defaultSourceArgs

-- | Default source arguments, e.g. optimisation settings.
defaultSourceArgs :: SourceArgs
defaultSourceArgs = SourceArgs
    { hsDefault  = mconcat [ stage0    ? arg "-O"
                           , notStage0 ? arg "-O2"
                           , arg "-H32m" ]
    , hsLibrary  = notStage0 ? arg "-haddock"
    , hsCompiler = mempty
    , hsGhc      = mempty }

-- Please update doc/flavours.md when changing the default build flavour.
-- | Default build flavour. Other build flavours are defined in modules
-- @Settings.Flavours.*@. Users can add new build flavours in "UserSettings".
defaultFlavour :: Flavour
defaultFlavour = Flavour
    { name               = "default"
    , extraArgs          = defaultExtraArgs
    , packages           = defaultPackages
    , bignumBackend      = defaultBignumBackend
    , bignumCheck        = False
    , libraryWays        = defaultLibraryWays
    , rtsWays            = defaultRtsWays
    , dynamicGhcPrograms = defaultDynamicGhcPrograms
    , ghciWithDebugger   = const False
    , ghcProfiled        = const False
    , ghcDebugged        = const False
    , ghcThreaded        = const True
    , ghcDebugAssertions = const False
    , ghcSplitSections   = False
    , ghcDocs            = cmdDocsArgs }

-- | Default logic for determining whether to build
--   dynamic GHC programs.
--
--   It corresponds to the DYNAMIC_GHC_PROGRAMS logic implemented
--   in @mk/config.mk.in@.
defaultDynamicGhcPrograms :: Action Bool
defaultDynamicGhcPrograms = do
  supportsShared <- platformSupportsSharedLibs
  return (not windowsHost && supportsShared)

-- | All 'Builder'-dependent command line arguments.
defaultBuilderArgs :: Args
defaultBuilderArgs = mconcat
    -- GHC-specific builders:
    [ alexBuilderArgs
    , cabalBuilderArgs
    , ccBuilderArgs
    , configureBuilderArgs
    , deriveConstantsBuilderArgs
    , genPrimopCodeBuilderArgs
    , ghcBuilderArgs
    , ghcPkgBuilderArgs
    , haddockBuilderArgs
    , happyBuilderArgs
    , hsc2hsBuilderArgs
    , hsCppBuilderArgs
    , ldBuilderArgs
    , arBuilderArgs
    , makeBuilderArgs
    , mergeObjectsBuilderArgs
    , runTestBuilderArgs
    , validateBuilderArgs
    , xelatexBuilderArgs
    , win32TarballsArgs
    , splitSectionsArgs
    -- Generic builders from the Hadrian library:
    , builder (Sphinx HtmlMode ) ? Hadrian.Builder.Sphinx.args HtmlMode
    , builder (Sphinx LatexMode) ? Hadrian.Builder.Sphinx.args LatexMode
    , builder (Sphinx ManMode  ) ? Hadrian.Builder.Sphinx.args ManMode
    , builder (Sphinx InfoMode ) ? Hadrian.Builder.Sphinx.args InfoMode
    , builder (Tar Create      ) ? Hadrian.Builder.Tar.args Create
    , builder (Tar Extract     ) ? Hadrian.Builder.Tar.args Extract
    , Hadrian.Builder.Git.gitArgs
    ]

-- | All 'Package'-dependent command line arguments.
defaultPackageArgs :: Args
defaultPackageArgs = mconcat [ packageArgs
                             , builder Ghc ? ghcWarningsArgs ]
