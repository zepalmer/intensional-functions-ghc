module GHC.Driver.Config.CoreToStg.Prep
  ( initCorePrepConfig
  , initCorePrepPgmConfig
  ) where

import GHC.Prelude

import GHC.Core.Opt.Pipeline.Types ( CoreToDo(..) )
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Config.Core.Lint
import GHC.Driver.Config.Core.Opt.Arity
import GHC.Tc.Utils.Env
import GHC.Types.Var
import GHC.Utils.Outputable ( alwaysQualify )

import GHC.CoreToStg.Prep

initCorePrepConfig :: HscEnv -> IO CorePrepConfig
initCorePrepConfig hsc_env = do
   let dflags = hsc_dflags hsc_env
   convertNumLit <- do
     let platform = targetPlatform dflags
         home_unit = hsc_home_unit hsc_env
         lookup_global = lookupGlobal hsc_env
     mkConvertNumLiteral platform home_unit lookup_global
   return $ CorePrepConfig
      { cp_catchNonexhaustiveCases = gopt Opt_CatchNonexhaustiveCases dflags
      , cp_convertNumLit = convertNumLit
      , cp_arityOpts = if gopt Opt_DoCleverArgEtaExpansion dflags
                       then Just (initArityOpts dflags)
                       else Nothing
      }

initCorePrepPgmConfig :: DynFlags -> [Var] -> CorePrepPgmConfig
initCorePrepPgmConfig dflags extra_vars = CorePrepPgmConfig
  { cpPgm_endPassConfig     = initEndPassConfig dflags extra_vars alwaysQualify CorePrep
  , cpPgm_generateDebugInfo = needSourceNotes dflags
  }
