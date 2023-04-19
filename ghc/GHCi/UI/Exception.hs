{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module GHCi.UI.Exception(printGhciException) where

import GHC.Prelude

import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Session

import GHC.Iface.Errors.Ppr
import GHC.Iface.Errors.Types

import GHC.Tc.Errors.Ppr
import GHC.Tc.Errors.Types

import GHC.Types.Error
import GHC.Types.SourceError

import GHC.Unit.State

import GHC.Utils.Logger
import GHC.Utils.Outputable

import Control.Monad.IO.Class


-- | Print the all diagnostics in a 'SourceError'.  Specialised for GHCi error reporting
-- for some error messages.
printGhciException :: (HasLogger m, MonadIO m, HasDynFlags m) => SourceError -> m ()
printGhciException err = do
  dflags <- getDynFlags
  logger <- getLogger
  let !diag_opts = initDiagOpts dflags
      !print_config = initPrintConfig dflags
  liftIO $ printMessages logger print_config diag_opts (GHCiMessage <$> (srcErrorMessages err))


newtype GHCiMessage = GHCiMessage { _getGhciMessage :: GhcMessage }

instance Diagnostic GHCiMessage where
  type DiagnosticOpts GHCiMessage = DiagnosticOpts GhcMessage

  defaultDiagnosticOpts = defaultDiagnosticOpts @GhcMessage

  diagnosticMessage opts (GHCiMessage msg) = ghciDiagnosticMessage opts msg

  diagnosticReason (GHCiMessage msg) = diagnosticReason msg

  diagnosticHints (GHCiMessage msg) = diagnosticHints msg

  diagnosticCode (GHCiMessage msg)  = diagnosticCode msg

-- Modifications to error messages which we want to display in GHCi
ghciDiagnosticMessage :: GhcMessageOpts -> GhcMessage -> DecoratedSDoc
ghciDiagnosticMessage ghc_opts msg =
  case msg of
    GhcTcRnMessage (TcRnInterfaceError err) ->
      case ghciInterfaceError err of
        Just sdoc -> mkSimpleDecorated sdoc
        Nothing -> diagnosticMessage ghc_opts msg
    GhcDriverMessage  (DriverInterfaceError err) ->
      case ghciInterfaceError err of
        Just sdoc -> mkSimpleDecorated sdoc
        Nothing -> diagnosticMessage ghc_opts msg
    _ -> diagnosticMessage ghc_opts msg
  where
    opts = tcOptsIfaceOpts (tcMessageOpts ghc_opts)

    ghciInterfaceError (Can'tFindInterface err looking_for) =
      hangNotEmpty (lookingForHerald looking_for) 2 <$> ghciMissingInterfaceErrorDiagnostic err
    ghciInterfaceError _ = Nothing

    ghciMissingInterfaceErrorDiagnostic reason =
      case reason of
        CantFindErr us module_or_interface cfi -> Just (pprWithUnitState us $ cantFindErrorX pkg_hidden_hint may_show_locations module_or_interface cfi)
        _ -> Nothing
      where

        may_show_locations = mayShowLocations ":set -v" (ifaceShowTriedFiles opts)

        pkg_hidden_hint = pkgHiddenHint hidden_msg (ifaceBuildingCabalPackage opts)
          where
            hidden_msg pkg =
              text "You can run" <+>
              quotes (text ":set -package " <> ppr (unitPackageName pkg)) <+>
              text "to expose it." $$
              text "(Note: this unloads all the modules in the current scope.)"
