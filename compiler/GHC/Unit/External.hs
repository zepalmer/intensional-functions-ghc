{-# LANGUAGE ExistentialQuantification #-}
module GHC.Unit.External
   ( ExternalUnitCache (..)
   , initExternalUnitCache
   , ExternalPackageState (..)
   , initExternalPackageState
   , EpsStats(..)
   , addEpsInStats
   , PackageTypeEnv
   , PackageIfaceTable
   , PackageInstEnv
   , PackageFamInstEnv
   , PackageRuleBase
   , PackageCompleteMatches
   , emptyPackageIfaceTable

   , extendPITFake
   , extendPIT
   , elemPIT
   , pitKeys
   , lookupPIT
   )
where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.Module.ModIface

import GHC.Core         ( RuleBase )
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( InstEnv, emptyInstEnv )
import GHC.Core.Opt.ConstantFold
import GHC.Core.Rules (mkRuleBase)

import GHC.Types.Annotations ( AnnEnv, emptyAnnEnv )
import GHC.Types.CompleteMatch
import GHC.Types.TypeEnv
import GHC.Types.Unique.DSet
import GHC.Types.Unique.FM

import Data.IORef
import GHC.Compact


type PackageTypeEnv          = TypeEnv
type PackageRuleBase         = RuleBase
type PackageInstEnv          = InstEnv
type PackageFamInstEnv       = FamInstEnv
type PackageAnnEnv           = AnnEnv
type PackageCompleteMatches = CompleteMatches

data CompactRegion = forall a . CompactRegion (Compact a) | EmptyRegion

-- | Helps us find information about modules in the imported packages
data PackageIfaceTable = PackageIfaceTable CompactRegion (ModuleEnv ModIface)
        -- Domain = modules in the imported packages

-- | Constructs an empty PackageIfaceTable
emptyPackageIfaceTable :: PackageIfaceTable
emptyPackageIfaceTable = PackageIfaceTable EmptyRegion emptyModuleEnv


lookupPIT :: PackageIfaceTable -> Module -> Maybe ModIface
lookupPIT (PackageIfaceTable _ pit) m = lookupModuleEnv pit m

extendPIT :: PackageIfaceTable -> Module -> ModIface -> IO PackageIfaceTable
extendPIT (PackageIfaceTable comp pit) m mi = do
  let raw_iface = forgetModIfaceCaches mi
  compact_region <- case comp of
    CompactRegion c -> do
      compactAdd c raw_iface
    EmptyRegion -> do
      compact raw_iface
  let compacted_iface = initModIfaceCaches $ getCompact compact_region
  return (PackageIfaceTable (CompactRegion compact_region) (extendModuleEnv pit m compacted_iface))

extendPITFake :: PackageIfaceTable -> Module -> PackageIfaceTable
extendPITFake (PackageIfaceTable c pit) mod =
  let fake_iface = emptyFullModIface mod
  in PackageIfaceTable c (extendModuleEnv pit mod fake_iface)

elemPIT :: Module -> PackageIfaceTable  -> Bool
elemPIT m (PackageIfaceTable _ pit) = elemModuleEnv m pit

pitKeys :: PackageIfaceTable -> [Module]
pitKeys (PackageIfaceTable _ pit) = moduleEnvKeys pit

-- | Information about the currently loaded external packages.
-- This is mutable because packages will be demand-loaded during
-- a compilation run as required.
newtype ExternalUnitCache = ExternalUnitCache
  { euc_eps :: IORef ExternalPackageState
  }

initExternalUnitCache :: IO ExternalUnitCache
initExternalUnitCache = ExternalUnitCache <$> newIORef initExternalPackageState

initExternalPackageState :: ExternalPackageState
initExternalPackageState = EPS
  { eps_is_boot          = emptyUFM
  , eps_PIT              = emptyPackageIfaceTable
  , eps_free_holes       = emptyInstalledModuleEnv
  , eps_PTE              = emptyTypeEnv
  , eps_inst_env         = emptyInstEnv
  , eps_fam_inst_env     = emptyFamInstEnv
  , eps_rule_base        = mkRuleBase builtinRules
  , -- Initialise the EPS rule pool with the built-in rules
    eps_mod_fam_inst_env = emptyModuleEnv
  , eps_complete_matches = []
  , eps_ann_env          = emptyAnnEnv
  , eps_stats            = EpsStats
                            { n_ifaces_in = 0
                            , n_decls_in = 0
                            , n_decls_out = 0
                            , n_insts_in = 0
                            , n_insts_out = 0
                            , n_rules_in = length builtinRules
                            , n_rules_out = 0
                            }
  }


-- | Information about other packages that we have slurped in by reading
-- their interface files
data ExternalPackageState
  = EPS {
        eps_is_boot :: !(ModuleNameEnv ModuleNameWithIsBoot),
                -- ^ In OneShot mode (only), home-package modules
                -- accumulate in the external package state, and are
                -- sucked in lazily.  For these home-pkg modules
                -- (only) we need to record which are boot modules.
                -- We set this field after loading all the
                -- explicitly-imported interfaces, but before doing
                -- anything else
                --
                -- The 'ModuleName' part is not necessary, but it's useful for
                -- debug prints, and it's convenient because this field comes
                -- direct from 'GHC.Tc.Utils.imp_dep_mods'

        eps_PIT :: !PackageIfaceTable,
                -- ^ The 'ModIface's for modules in external packages
                -- whose interfaces we have opened.
                -- The declarations in these interface files are held in the
                -- 'eps_decls', 'eps_inst_env', 'eps_fam_inst_env' and 'eps_rules'
                -- fields of this record, not in the 'mi_decls' fields of the
                -- interface we have sucked in.
                --
                -- What /is/ in the PIT is:
                --
                -- * The Module
                --
                -- * Fingerprint info
                --
                -- * Its exports
                --
                -- * Fixities
                --
                -- * Deprecations and warnings

        eps_free_holes :: InstalledModuleEnv (UniqDSet ModuleName),
                -- ^ Cache for 'mi_free_holes'.  Ordinarily, we can rely on
                -- the 'eps_PIT' for this information, EXCEPT that when
                -- we do dependency analysis, we need to look at the
                -- 'Dependencies' of our imports to determine what their
                -- precise free holes are ('moduleFreeHolesPrecise').  We
                -- don't want to repeatedly reread in the interface
                -- for every import, so cache it here.  When the PIT
                -- gets filled in we can drop these entries.

        eps_PTE :: !PackageTypeEnv,
                -- ^ Result of typechecking all the external package
                -- interface files we have sucked in. The domain of
                -- the mapping is external-package modules

        eps_inst_env     :: !PackageInstEnv,   -- ^ The total 'InstEnv' accumulated
                                               -- from all the external-package modules
        eps_fam_inst_env :: !PackageFamInstEnv,-- ^ The total 'FamInstEnv' accumulated
                                               -- from all the external-package modules
        eps_rule_base    :: !PackageRuleBase,  -- ^ The total 'RuleEnv' accumulated
                                               -- from all the external-package modules
        eps_ann_env      :: !PackageAnnEnv,    -- ^ The total 'AnnEnv' accumulated
                                               -- from all the external-package modules
        eps_complete_matches :: !PackageCompleteMatches,
                                  -- ^ The total 'CompleteMatches' accumulated
                                  -- from all the external-package modules

        eps_mod_fam_inst_env :: !(ModuleEnv FamInstEnv), -- ^ The family instances accumulated from external
                                                         -- packages, keyed off the module that declared them

        eps_stats :: !EpsStats                 -- ^ Stastics about what was loaded from external packages
  }

-- | Accumulated statistics about what we are putting into the 'ExternalPackageState'.
-- \"In\" means stuff that is just /read/ from interface files,
-- \"Out\" means actually sucked in and type-checked
data EpsStats = EpsStats { n_ifaces_in
                         , n_decls_in, n_decls_out
                         , n_rules_in, n_rules_out
                         , n_insts_in, n_insts_out :: !Int }

addEpsInStats :: EpsStats -> Int -> Int -> Int -> EpsStats
-- ^ Add stats for one newly-read interface
addEpsInStats stats n_decls n_insts n_rules
  = stats { n_ifaces_in = n_ifaces_in stats + 1
          , n_decls_in  = n_decls_in stats + n_decls
          , n_insts_in  = n_insts_in stats + n_insts
          , n_rules_in  = n_rules_in stats + n_rules }

