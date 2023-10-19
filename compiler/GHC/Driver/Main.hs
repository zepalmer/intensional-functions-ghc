{-# LANGUAGE CPP                      #-}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE ViewPatterns             #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

-------------------------------------------------------------------------------
--
-- | Main API for compiling plain Haskell source code.
--
-- This module implements compilation of a Haskell source. It is
-- /not/ concerned with preprocessing of source files; this is handled
-- in "GHC.Driver.Pipeline"
--
-- There are various entry points depending on what mode we're in:
-- "batch" mode (@--make@), "one-shot" mode (@-c@, @-S@ etc.), and
-- "interactive" mode (GHCi). There are also entry points for
-- individual passes: parsing, typechecking/renaming, desugaring, and
-- simplification.
--
-- All the functions here take an 'HscEnv' as a parameter, but none of
-- them return a new one: 'HscEnv' is treated as an immutable value
-- from here on in (although it has mutable components, for the
-- caches).
--
-- We use the Hsc monad to deal with warning messages consistently:
-- specifically, while executing within an Hsc monad, warnings are
-- collected. When a Hsc monad returns to an IO monad, the
-- warnings are printed, or compilation aborts if the @-Werror@
-- flag is enabled.
--
-- (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
--
-------------------------------------------------------------------------------

module GHC.Driver.Main
    (
    -- * Making an HscEnv
      newHscEnv

    -- * Compiling complete source files
    , Messager, batchMsg
    , HscStatus (..)
    , hscIncrementalCompile
    , initModDetails
    , hscMaybeWriteIface
    , hscCompileCmmFile

    , hscGenHardCode
    , hscInteractive

    -- * Running passes separately
    , hscParse
    , hscTypecheckRename
    , hscDesugar
    , makeSimpleDetails
    , hscSimplify -- ToDo, shouldn't really export this

    -- * Safe Haskell
    , hscCheckSafe
    , hscGetSafe

    -- * Support for interactive evaluation
    , hscParseIdentifier
    , hscTcRcLookupName
    , hscTcRnGetInfo
    , hscIsGHCiMonad
    , hscGetModuleInterface
    , hscRnImportDecls
    , hscTcRnLookupRdrName
    , hscStmt, hscParseStmtWithLocation, hscStmtWithLocation, hscParsedStmt
    , hscDecls, hscParseDeclsWithLocation, hscDeclsWithLocation, hscParsedDecls
    , hscTcExpr, TcRnExprMode(..), hscImport, hscKcType
    , hscParseExpr
    , hscParseType
    , hscCompileCoreExpr
    -- * Low-level exports for hooks
    , hscCompileCoreExpr'
      -- We want to make sure that we export enough to be able to redefine
      -- hsc_typecheck in client code
    , hscParse', hscSimplify', hscDesugar', tcRnModule', doCodeGen
    , getHscEnv
    , hscSimpleIface'
    , oneShotMsg
    , dumpIfaceStats
    , ioMsgMaybe
    , showModuleIndex
    , hscAddSptEntries
    ) where

import GHC.Prelude

import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.CodeOutput
import GHC.Driver.Config
import GHC.Driver.Hooks
import GHC.Parser.Errors

import GHC.Runtime.Context
import GHC.Runtime.Interpreter ( addSptEntry, hscInterp )
import GHC.Runtime.Loader      ( initializePlugins )
import GHCi.RemoteTypes        ( ForeignHValue )
import GHC.ByteCode.Types

import GHC.Linker.Loader
import GHC.Linker.Types

import GHC.Hs
import GHC.Hs.Dump
import GHC.Hs.Stats         ( ppSourceStats )

import GHC.HsToCore

import GHC.StgToByteCode    ( byteCodeGen )

import GHC.IfaceToCore  ( typecheckIface )

import GHC.Iface.Load   ( ifaceStats, initExternalPackageState, writeIface )
import GHC.Iface.Make
import GHC.Iface.Recomp
import GHC.Iface.Tidy
import GHC.Iface.Ext.Ast    ( mkHieFile )
import GHC.Iface.Ext.Types  ( getAsts, hie_asts, hie_module )
import GHC.Iface.Ext.Binary ( readHieFile, writeHieFile , hie_file_result, NameCacheUpdater(..))
import GHC.Iface.Ext.Debug  ( diffFile, validateScopes )
import GHC.Iface.Env        ( updNameCache )

import GHC.Core
import GHC.Core.Tidy           ( tidyExpr )
import GHC.Core.Type           ( Type, Kind )
import GHC.Core.Lint           ( lintInteractiveExpr )
import GHC.Core.Multiplicity
import GHC.Core.Utils          ( exprType )
import GHC.Core.ConLike
import GHC.Core.Opt.Pipeline
import GHC.Core.TyCon
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv

import GHC.CoreToStg.Prep
import GHC.CoreToStg    ( coreToStg )

import GHC.Parser.Errors.Ppr
import GHC.Parser
import GHC.Parser.Lexer as Lexer

import GHC.Tc.Module
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Zonk    ( ZonkFlexi (DefaultFlexi) )

import GHC.Stg.Syntax
import GHC.Stg.FVs      ( annTopBindingsFreeVars )
import GHC.Stg.Pipeline ( stg2stg )

import GHC.Builtin.Utils
import GHC.Builtin.Names
import GHC.Builtin.Uniques ( mkPseudoUniqueE )

import qualified GHC.StgToCmm as StgToCmm ( codeGen )
import GHC.StgToCmm.Types (CgInfos (..), ModuleLFInfos)

import GHC.Cmm
import GHC.Cmm.Parser       ( parseCmmFile )
import GHC.Cmm.Info.Build
import GHC.Cmm.Pipeline
import GHC.Cmm.Info

import GHC.Unit
import GHC.Unit.External
import GHC.Unit.State
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Graph
import GHC.Unit.Module.Imported
import GHC.Unit.Module.Deps
import GHC.Unit.Module.Status
import GHC.Unit.Home.ModInfo

import GHC.Types.Id
import GHC.Types.SourceError
import GHC.Types.SafeHaskell
import GHC.Types.ForeignStubs
import GHC.Types.Var.Env       ( emptyTidyEnv )
import GHC.Types.Error
import GHC.Types.Fixity.Env
import GHC.Types.CostCentre
import GHC.Types.IPE
import GHC.Types.Unique.Supply
import GHC.Types.SourceFile
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Cache ( initNameCache )
import GHC.Types.Name.Reader
import GHC.Types.Name.Ppr
import GHC.Types.TyThing
import GHC.Types.HpcInfo

import GHC.Utils.Fingerprint ( Fingerprint )
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Exception
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Data.FastString
import GHC.Data.Bag
import GHC.Data.StringBuffer
import qualified GHC.Data.Stream as Stream
import GHC.Data.Stream (Stream)

import Data.Data hiding (Fixity, TyCon)
import Data.List        ( nub, isPrefixOf, partition )
import Control.Monad
import Data.IORef
import System.FilePath as FilePath
import System.Directory
import System.IO (fixIO)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Functor
import Control.DeepSeq (force)
import Data.Bifunctor (first, bimap)
import GHC.Data.Maybe
import Data.List.NonEmpty (NonEmpty ((:|)))

#include "HsVersions.h"


{- **********************************************************************
%*                                                                      *
                Initialisation
%*                                                                      *
%********************************************************************* -}

newHscEnv :: DynFlags -> IO HscEnv
newHscEnv dflags = do
    -- we don't store the unit databases and the unit state to still
    -- allow `setSessionDynFlags` to be used to set unit db flags.
    eps_var <- newIORef initExternalPackageState
    us      <- mkSplitUniqSupply 'r'
    nc_var  <- newIORef (initNameCache us knownKeyNames)
    fc_var  <- newIORef emptyInstalledModuleEnv
    logger  <- initLogger
    tmpfs   <- initTmpFs
    -- FIXME: it's sad that we have so many "unitialized" fields filled with
    -- empty stuff or lazy panics. We should have two kinds of HscEnv
    -- (initialized or not) instead and less fields that are mutable over time.
    return HscEnv {  hsc_dflags         = dflags
                  ,  hsc_logger         = logger
                  ,  hsc_targets        = []
                  ,  hsc_mod_graph      = emptyMG
                  ,  hsc_IC             = emptyInteractiveContext dflags
                  ,  hsc_HPT            = emptyHomePackageTable
                  ,  hsc_EPS            = eps_var
                  ,  hsc_NC             = nc_var
                  ,  hsc_FC             = fc_var
                  ,  hsc_type_env_var   = Nothing
                  ,  hsc_interp         = Nothing
                  ,  hsc_unit_env       = panic "hsc_unit_env not initialized"
                  ,  hsc_plugins        = []
                  ,  hsc_static_plugins = []
                  ,  hsc_unit_dbs       = Nothing
                  ,  hsc_hooks          = emptyHooks
                  ,  hsc_tmpfs          = tmpfs
                  }

-- -----------------------------------------------------------------------------

getWarnings :: Hsc WarningMessages
getWarnings = Hsc $ \_ w -> return (w, w)

clearWarnings :: Hsc ()
clearWarnings = Hsc $ \_ _ -> return ((), emptyBag)

logWarnings :: WarningMessages -> Hsc ()
logWarnings w = Hsc $ \_ w0 -> return ((), w0 `unionBags` w)

getHscEnv :: Hsc HscEnv
getHscEnv = Hsc $ \e w -> return (e, w)

handleWarnings :: Hsc ()
handleWarnings = do
    dflags <- getDynFlags
    logger <- getLogger
    w <- getWarnings
    liftIO $ printOrThrowWarnings logger dflags w
    clearWarnings

-- | log warning in the monad, and if there are errors then
-- throw a SourceError exception.
logWarningsReportErrors :: (Bag PsWarning, Bag PsError) -> Hsc ()
logWarningsReportErrors (warnings,errors) = do
    let warns = fmap pprWarning warnings
        errs  = fmap pprError   errors
    logWarnings warns
    when (not $ isEmptyBag errs) $ throwErrors errs

-- | Log warnings and throw errors, assuming the messages
-- contain at least one error (e.g. coming from PFailed)
handleWarningsThrowErrors :: (Bag PsWarning, Bag PsError) -> Hsc a
handleWarningsThrowErrors (warnings, errors) = do
    let warns = fmap pprWarning warnings
        errs  = fmap pprError   errors
    logWarnings warns
    dflags <- getDynFlags
    logger <- getLogger
    (wWarns, wErrs) <- warningsToMessages dflags <$> getWarnings
    liftIO $ printBagOfErrors logger dflags wWarns
    throwErrors (unionBags errs wErrs)

-- | Deal with errors and warnings returned by a compilation step
--
-- In order to reduce dependencies to other parts of the compiler, functions
-- outside the "main" parts of GHC return warnings and errors as a parameter
-- and signal success via by wrapping the result in a 'Maybe' type. This
-- function logs the returned warnings and propagates errors as exceptions
-- (of type 'SourceError').
--
-- This function assumes the following invariants:
--
--  1. If the second result indicates success (is of the form 'Just x'),
--     there must be no error messages in the first result.
--
--  2. If there are no error messages, but the second result indicates failure
--     there should be warnings in the first result. That is, if the action
--     failed, it must have been due to the warnings (i.e., @-Werror@).
ioMsgMaybe :: IO (Messages DecoratedSDoc, Maybe a) -> Hsc a
ioMsgMaybe ioA = do
    (msgs, mb_r) <- liftIO ioA
    let (warns, errs) = partitionMessages msgs
    logWarnings warns
    case mb_r of
        Nothing -> throwErrors errs
        Just r  -> ASSERT( isEmptyBag errs ) return r

-- | like ioMsgMaybe, except that we ignore error messages and return
-- 'Nothing' instead.
ioMsgMaybe' :: IO (Messages DecoratedSDoc, Maybe a) -> Hsc (Maybe a)
ioMsgMaybe' ioA = do
    (msgs, mb_r) <- liftIO $ ioA
    logWarnings (getWarningMessages msgs)
    return mb_r

-- -----------------------------------------------------------------------------
-- | Lookup things in the compiler's environment

hscTcRnLookupRdrName :: HscEnv -> LocatedN RdrName -> IO [Name]
hscTcRnLookupRdrName hsc_env0 rdr_name
  = runInteractiveHsc hsc_env0 $
    do { hsc_env <- getHscEnv
       ; ioMsgMaybe $ tcRnLookupRdrName hsc_env rdr_name }

hscTcRcLookupName :: HscEnv -> Name -> IO (Maybe TyThing)
hscTcRcLookupName hsc_env0 name = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe' $ tcRnLookupName hsc_env name
      -- ignore errors: the only error we're likely to get is
      -- "name not found", and the Maybe in the return type
      -- is used to indicate that.

hscTcRnGetInfo :: HscEnv -> Name
               -> IO (Maybe (TyThing, Fixity, [ClsInst], [FamInst], SDoc))
hscTcRnGetInfo hsc_env0 name
  = runInteractiveHsc hsc_env0 $
    do { hsc_env <- getHscEnv
       ; ioMsgMaybe' $ tcRnGetInfo hsc_env name }

hscIsGHCiMonad :: HscEnv -> String -> IO Name
hscIsGHCiMonad hsc_env name
  = runHsc hsc_env $ ioMsgMaybe $ isGHCiMonad hsc_env name

hscGetModuleInterface :: HscEnv -> Module -> IO ModIface
hscGetModuleInterface hsc_env0 mod = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ getModuleInterface hsc_env mod

-- -----------------------------------------------------------------------------
-- | Rename some import declarations
hscRnImportDecls :: HscEnv -> [LImportDecl GhcPs] -> IO GlobalRdrEnv
hscRnImportDecls hsc_env0 import_decls = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ tcRnImportDecls hsc_env import_decls

-- -----------------------------------------------------------------------------
-- | parse a file, returning the abstract syntax

hscParse :: HscEnv -> ModSummary -> IO HsParsedModule
hscParse hsc_env mod_summary = runHsc hsc_env $ hscParse' mod_summary

-- internal version, that doesn't fail due to -Werror
hscParse' :: ModSummary -> Hsc HsParsedModule
hscParse' mod_summary
 | Just r <- ms_parsed_mod mod_summary = return r
 | otherwise = do
    dflags <- getDynFlags
    logger <- getLogger
    {-# SCC "Parser" #-} withTiming logger dflags
                (text "Parser"<+>brackets (ppr $ ms_mod mod_summary))
                (const ()) $ do
    let src_filename  = ms_hspp_file mod_summary
        maybe_src_buf = ms_hspp_buf  mod_summary

    --------------------------  Parser  ----------------
    -- sometimes we already have the buffer in memory, perhaps
    -- because we needed to parse the imports out of it, or get the
    -- module name.
    buf <- case maybe_src_buf of
               Just b  -> return b
               Nothing -> liftIO $ hGetStringBuffer src_filename

    let loc = mkRealSrcLoc (mkFastString src_filename) 1 1

    when (wopt Opt_WarnUnicodeBidirectionalFormatCharacters dflags) $ do
      case checkBidirectionFormatChars (PsLoc loc (BufPos 0)) buf of
        Nothing -> pure ()
        Just chars ->
          logWarnings $ unitBag $ pprWarning $
               PsWarnBidirectionalFormatChars chars

    let parseMod | HsigFile == ms_hsc_src mod_summary
                 = parseSignature
                 | otherwise = parseModule

    case unP parseMod (initParserState (initParserOpts dflags) buf loc) of
        PFailed pst ->
            handleWarningsThrowErrors (getMessages pst)
        POk pst rdr_module -> do
            let (warns, errs) = bimap (fmap pprWarning) (fmap pprError) (getMessages pst)
            logWarnings warns
            liftIO $ dumpIfSet_dyn logger dflags Opt_D_dump_parsed "Parser"
                        FormatHaskell (ppr rdr_module)
            liftIO $ dumpIfSet_dyn logger dflags Opt_D_dump_parsed_ast "Parser AST"
                        FormatHaskell (showAstData NoBlankSrcSpan
                                                   NoBlankEpAnnotations
                                                   rdr_module)
            liftIO $ dumpIfSet_dyn logger dflags Opt_D_source_stats "Source Statistics"
                        FormatText (ppSourceStats False rdr_module)
            when (not $ isEmptyBag errs) $ throwErrors errs

            -- To get the list of extra source files, we take the list
            -- that the parser gave us,
            --   - eliminate files beginning with '<'.  gcc likes to use
            --     pseudo-filenames like "<built-in>" and "<command-line>"
            --   - normalise them (eliminate differences between ./f and f)
            --   - filter out the preprocessed source file
            --   - filter out anything beginning with tmpdir
            --   - remove duplicates
            --   - filter out the .hs/.lhs source filename if we have one
            --
            let n_hspp  = FilePath.normalise src_filename
                srcs0 = nub $ filter (not . (tmpDir dflags `isPrefixOf`))
                            $ filter (not . (== n_hspp))
                            $ map FilePath.normalise
                            $ filter (not . isPrefixOf "<")
                            $ map unpackFS
                            $ srcfiles pst
                srcs1 = case ml_hs_file (ms_location mod_summary) of
                          Just f  -> filter (/= FilePath.normalise f) srcs0
                          Nothing -> srcs0

            -- sometimes we see source files from earlier
            -- preprocessing stages that cannot be found, so just
            -- filter them out:
            srcs2 <- liftIO $ filterM doesFileExist srcs1

            let res = HsParsedModule {
                      hpm_module    = rdr_module,
                      hpm_src_files = srcs2
                   }

            -- apply parse transformation of plugins
            let applyPluginAction p opts
                  = parsedResultAction p opts mod_summary
            hsc_env <- getHscEnv
            withPlugins hsc_env applyPluginAction res

checkBidirectionFormatChars :: PsLoc -> StringBuffer -> Maybe (NonEmpty (PsLoc, Char, String))
checkBidirectionFormatChars start_loc sb
  | containsBidirectionalFormatChar sb = Just $ go start_loc sb
  | otherwise = Nothing
  where
    go :: PsLoc -> StringBuffer -> NonEmpty (PsLoc, Char, String)
    go loc sb
      | atEnd sb = panic "checkBidirectionFormatChars: no char found"
      | otherwise = case nextChar sb of
          (chr, sb)
            | Just desc <- lookup chr bidirectionalFormatChars ->
                (loc, chr, desc) :| go1 (advancePsLoc loc chr) sb
            | otherwise -> go (advancePsLoc loc chr) sb

    go1 :: PsLoc -> StringBuffer -> [(PsLoc, Char, String)]
    go1 loc sb
      | atEnd sb = []
      | otherwise = case nextChar sb of
          (chr, sb)
            | Just desc <- lookup chr bidirectionalFormatChars ->
                (loc, chr, desc) : go1 (advancePsLoc loc chr) sb
            | otherwise -> go1 (advancePsLoc loc chr) sb


-- -----------------------------------------------------------------------------
-- | If the renamed source has been kept, extract it. Dump it if requested.


extract_renamed_stuff :: ModSummary -> TcGblEnv -> Hsc RenamedStuff
extract_renamed_stuff mod_summary tc_result = do
    let rn_info = getRenamedStuff tc_result

    dflags <- getDynFlags
    logger <- getLogger
    liftIO $ dumpIfSet_dyn logger dflags Opt_D_dump_rn_ast "Renamer"
                FormatHaskell (showAstData NoBlankSrcSpan NoBlankEpAnnotations rn_info)

    -- Create HIE files
    when (gopt Opt_WriteHie dflags) $ do
        -- I assume this fromJust is safe because `-fwrite-hie-file`
        -- enables the option which keeps the renamed source.
        hieFile <- mkHieFile mod_summary tc_result (fromJust rn_info)
        let out_file = ml_hie_file $ ms_location mod_summary
        liftIO $ writeHieFile out_file hieFile
        liftIO $ dumpIfSet_dyn logger dflags Opt_D_dump_hie "HIE AST" FormatHaskell (ppr $ hie_asts hieFile)

        -- Validate HIE files
        when (gopt Opt_ValidateHie dflags) $ do
            hs_env <- Hsc $ \e w -> return (e, w)
            liftIO $ do
              -- Validate Scopes
              case validateScopes (hie_module hieFile) $ getAsts $ hie_asts hieFile of
                  [] -> putMsg logger dflags $ text "Got valid scopes"
                  xs -> do
                    putMsg logger dflags $ text "Got invalid scopes"
                    mapM_ (putMsg logger dflags) xs
              -- Roundtrip testing
              file' <- readHieFile (NCU $ updNameCache $ hsc_NC hs_env) out_file
              case diffFile hieFile (hie_file_result file') of
                [] ->
                  putMsg logger dflags $ text "Got no roundtrip errors"
                xs -> do
                  putMsg logger dflags $ text "Got roundtrip errors"
                  mapM_ (putMsg logger (dopt_set dflags Opt_D_ppr_debug)) xs
    return rn_info


-- -----------------------------------------------------------------------------
-- | Rename and typecheck a module, additionally returning the renamed syntax
hscTypecheckRename :: HscEnv -> ModSummary -> HsParsedModule
                   -> IO (TcGblEnv, RenamedStuff)
hscTypecheckRename hsc_env mod_summary rdr_module = runHsc hsc_env $
    hsc_typecheck True mod_summary (Just rdr_module)


-- | A bunch of logic piled around @tcRnModule'@, concerning a) backpack
-- b) concerning dumping rename info and hie files. It would be nice to further
-- separate this stuff out, probably in conjunction better separating renaming
-- and type checking (#17781).
hsc_typecheck :: Bool -- ^ Keep renamed source?
              -> ModSummary -> Maybe HsParsedModule
              -> Hsc (TcGblEnv, RenamedStuff)
hsc_typecheck keep_rn mod_summary mb_rdr_module = do
    hsc_env <- getHscEnv
    let hsc_src = ms_hsc_src mod_summary
        dflags = hsc_dflags hsc_env
        home_unit = hsc_home_unit hsc_env
        outer_mod = ms_mod mod_summary
        mod_name = moduleName outer_mod
        outer_mod' = mkHomeModule home_unit mod_name
        inner_mod = homeModuleNameInstantiation home_unit mod_name
        src_filename  = ms_hspp_file mod_summary
        real_loc = realSrcLocSpan $ mkRealSrcLoc (mkFastString src_filename) 1 1
        keep_rn' = gopt Opt_WriteHie dflags || keep_rn
    MASSERT( isHomeModule home_unit outer_mod )
    tc_result <- if hsc_src == HsigFile && not (isHoleModule inner_mod)
        then ioMsgMaybe $ tcRnInstantiateSignature hsc_env outer_mod' real_loc
        else
         do hpm <- case mb_rdr_module of
                    Just hpm -> return hpm
                    Nothing -> hscParse' mod_summary
            tc_result0 <- tcRnModule' mod_summary keep_rn' hpm
            if hsc_src == HsigFile
                then do (iface, _, _) <- liftIO $ hscSimpleIface hsc_env tc_result0 Nothing
                        ioMsgMaybe $
                            tcRnMergeSignatures hsc_env hpm tc_result0 iface
                else return tc_result0
    -- TODO are we extracting anything when we merely instantiate a signature?
    -- If not, try to move this into the "else" case above.
    rn_info <- extract_renamed_stuff mod_summary tc_result
    return (tc_result, rn_info)

-- wrapper around tcRnModule to handle safe haskell extras
tcRnModule' :: ModSummary -> Bool -> HsParsedModule
            -> Hsc TcGblEnv
tcRnModule' sum save_rn_syntax mod = do
    hsc_env <- getHscEnv
    dflags   <- getDynFlags

    -- -Wmissing-safe-haskell-mode
    when (not (safeHaskellModeEnabled dflags)
          && wopt Opt_WarnMissingSafeHaskellMode dflags) $
        logWarnings $ unitBag $
        makeIntoWarning (Reason Opt_WarnMissingSafeHaskellMode) $
        mkPlainWarnMsg (getLoc (hpm_module mod)) $
        warnMissingSafeHaskellMode

    tcg_res <- {-# SCC "Typecheck-Rename" #-}
               ioMsgMaybe $
                   tcRnModule hsc_env sum
                     save_rn_syntax mod

    -- See Note [Safe Haskell Overlapping Instances Implementation]
    -- although this is used for more than just that failure case.
    (tcSafeOK, whyUnsafe) <- liftIO $ readIORef (tcg_safeInfer tcg_res)
    let allSafeOK = safeInferred dflags && tcSafeOK

    -- end of the safe haskell line, how to respond to user?
    if not (safeHaskellOn dflags)
         || (safeInferOn dflags && not allSafeOK)
      -- if safe Haskell off or safe infer failed, mark unsafe
      then markUnsafeInfer tcg_res whyUnsafe

      -- module (could be) safe, throw warning if needed
      else do
          tcg_res' <- hscCheckSafeImports tcg_res
          safe <- liftIO $ fst <$> readIORef (tcg_safeInfer tcg_res')
          when safe $
            case wopt Opt_WarnSafe dflags of
              True
                | safeHaskell dflags == Sf_Safe -> return ()
                | otherwise -> (logWarnings $ unitBag $
                       makeIntoWarning (Reason Opt_WarnSafe) $
                       mkPlainWarnMsg (warnSafeOnLoc dflags) $
                       errSafe tcg_res')
              False | safeHaskell dflags == Sf_Trustworthy &&
                      wopt Opt_WarnTrustworthySafe dflags ->
                      (logWarnings $ unitBag $
                       makeIntoWarning (Reason Opt_WarnTrustworthySafe) $
                       mkPlainWarnMsg (trustworthyOnLoc dflags) $
                       errTwthySafe tcg_res')
              False -> return ()
          return tcg_res'
  where
    pprMod t  = ppr $ moduleName $ tcg_mod t
    errSafe t = quotes (pprMod t) <+> text "has been inferred as safe!"
    errTwthySafe t = quotes (pprMod t)
      <+> text "is marked as Trustworthy but has been inferred as safe!"
    warnMissingSafeHaskellMode = ppr (moduleName (ms_mod sum))
      <+> text "is missing Safe Haskell mode"

-- | Convert a typechecked module to Core
hscDesugar :: HscEnv -> ModSummary -> TcGblEnv -> IO ModGuts
hscDesugar hsc_env mod_summary tc_result =
    runHsc hsc_env $ hscDesugar' (ms_location mod_summary) tc_result

hscDesugar' :: ModLocation -> TcGblEnv -> Hsc ModGuts
hscDesugar' mod_location tc_result = do
    hsc_env <- getHscEnv
    r <- ioMsgMaybe $
      {-# SCC "deSugar" #-}
      deSugar hsc_env mod_location tc_result

    -- always check -Werror after desugaring, this is the last opportunity for
    -- warnings to arise before the backend.
    handleWarnings
    return r

-- | Make a 'ModDetails' from the results of typechecking. Used when
-- typechecking only, as opposed to full compilation.
makeSimpleDetails :: HscEnv -> TcGblEnv -> IO ModDetails
makeSimpleDetails hsc_env tc_result = mkBootModDetailsTc hsc_env tc_result


{- **********************************************************************
%*                                                                      *
                The main compiler pipeline
%*                                                                      *
%********************************************************************* -}

{-
                   --------------------------------
                        The compilation proper
                   --------------------------------

It's the task of the compilation proper to compile Haskell, hs-boot and core
files to either byte-code, hard-code (C, asm, LLVM, etc.) or to nothing at all
(the module is still parsed and type-checked. This feature is mostly used by
IDE's and the likes). Compilation can happen in either 'one-shot', 'batch',
'nothing', or 'interactive' mode. 'One-shot' mode targets hard-code, 'batch'
mode targets hard-code, 'nothing' mode targets nothing and 'interactive' mode
targets byte-code.

The modes are kept separate because of their different types and meanings:

 * In 'one-shot' mode, we're only compiling a single file and can therefore
 discard the new ModIface and ModDetails. This is also the reason it only
 targets hard-code; compiling to byte-code or nothing doesn't make sense when
 we discard the result.

 * 'Batch' mode is like 'one-shot' except that we keep the resulting ModIface
 and ModDetails. 'Batch' mode doesn't target byte-code since that require us to
 return the newly compiled byte-code.

 * 'Nothing' mode has exactly the same type as 'batch' mode but they're still
 kept separate. This is because compiling to nothing is fairly special: We
 don't output any interface files, we don't run the simplifier and we don't
 generate any code.

 * 'Interactive' mode is similar to 'batch' mode except that we return the
 compiled byte-code together with the ModIface and ModDetails.

Trying to compile a hs-boot file to byte-code will result in a run-time error.
This is the only thing that isn't caught by the type-system.
-}


type Messager = HscEnv -> (Int,Int) -> RecompileRequired -> ModuleGraphNode -> IO ()

-- | This function runs GHC's frontend with recompilation
-- avoidance. Specifically, it checks if recompilation is needed,
-- and if it is, it parses and typechecks the input module.
-- It does not write out the results of typechecking (See
-- compileOne and hscIncrementalCompile).
hscIncrementalFrontend :: Bool -- always do basic recompilation check?
                       -> Maybe TcGblEnv
                       -> Maybe Messager
                       -> ModSummary
                       -> SourceModified
                       -> Maybe ModIface  -- Old interface, if available
                       -> (Int,Int)       -- (i,n) = module i of n (for msgs)
                       -> Hsc (Either ModIface (FrontendResult, Maybe Fingerprint))

hscIncrementalFrontend
  always_do_basic_recompilation_check m_tc_result
  mHscMessage mod_summary source_modified mb_old_iface mod_index
    = do
    hsc_env <- getHscEnv

    let msg what = case mHscMessage of
          -- We use extendModSummaryNoDeps because extra backpack deps are only needed for batch mode
          Just hscMessage -> hscMessage hsc_env mod_index what (ModuleNode (extendModSummaryNoDeps mod_summary))
          Nothing -> return ()

        skip iface = do
            liftIO $ msg UpToDate
            return $ Left iface

        compile mb_old_hash reason = do
            liftIO $ msg reason
            tc_result <- case hscFrontendHook (hsc_hooks hsc_env) of
              Nothing -> FrontendTypecheck . fst <$> hsc_typecheck False mod_summary Nothing
              Just h  -> h mod_summary
            return $ Right (tc_result, mb_old_hash)

        stable = case source_modified of
                     SourceUnmodifiedAndStable -> True
                     _                         -> False

    case m_tc_result of
         Just tc_result
          | not always_do_basic_recompilation_check ->
             return $ Right (FrontendTypecheck tc_result, Nothing)
         _ -> do
            (recomp_reqd, mb_checked_iface)
                <- {-# SCC "checkOldIface" #-}
                   liftIO $ checkOldIface hsc_env mod_summary
                                source_modified mb_old_iface
            -- save the interface that comes back from checkOldIface.
            -- In one-shot mode we don't have the old iface until this
            -- point, when checkOldIface reads it from the disk.
            let mb_old_hash = fmap (mi_iface_hash . mi_final_exts) mb_checked_iface

            case mb_checked_iface of
                Just iface | not (recompileRequired recomp_reqd) ->
                    -- If the module used TH splices when it was last
                    -- compiled, then the recompilation check is not
                    -- accurate enough (#481) and we must ignore
                    -- it.  However, if the module is stable (none of
                    -- the modules it depends on, directly or
                    -- indirectly, changed), then we *can* skip
                    -- recompilation. This is why the SourceModified
                    -- type contains SourceUnmodifiedAndStable, and
                    -- it's pretty important: otherwise ghc --make
                    -- would always recompile TH modules, even if
                    -- nothing at all has changed. Stability is just
                    -- the same check that make is doing for us in
                    -- one-shot mode.
                    case m_tc_result of
                    Nothing
                     | mi_used_th iface && not stable ->
                        compile mb_old_hash (RecompBecause "TH")
                    _ ->
                        skip iface
                _ ->
                    case m_tc_result of
                    Nothing -> compile mb_old_hash recomp_reqd
                    Just tc_result ->
                        return $ Right (FrontendTypecheck tc_result, mb_old_hash)

--------------------------------------------------------------
-- Compilers
--------------------------------------------------------------

-- | Used by both OneShot and batch mode. Runs the pipeline HsSyn and Core parts
-- of the pipeline.
-- We return a interface if we already had an old one around and recompilation
-- was not needed. Otherwise it will be created during later passes when we
-- run the compilation pipeline.
hscIncrementalCompile :: Bool
                      -> Maybe TcGblEnv
                      -> Maybe Messager
                      -> HscEnv
                      -> ModSummary
                      -> SourceModified
                      -> Maybe ModIface
                      -> (Int,Int)
                      -> IO (HscStatus, HscEnv)
hscIncrementalCompile always_do_basic_recompilation_check m_tc_result
    mHscMessage hsc_env' mod_summary source_modified mb_old_iface mod_index
  = do
    hsc_env'' <- initializePlugins hsc_env'

    -- One-shot mode needs a knot-tying mutable variable for interface
    -- files. See GHC.Tc.Utils.TcGblEnv.tcg_type_env_var.
    -- See also Note [hsc_type_env_var hack]
    type_env_var <- newIORef emptyNameEnv
    let mod = ms_mod mod_summary
        hsc_env | isOneShot (ghcMode (hsc_dflags hsc_env''))
                = hsc_env'' { hsc_type_env_var = Just (mod, type_env_var) }
                | otherwise
                = hsc_env''

    -- NB: enter Hsc monad here so that we don't bail out early with
    -- -Werror on typechecker warnings; we also want to run the desugarer
    -- to get those warnings too. (But we'll always exit at that point
    -- because the desugarer runs ioMsgMaybe.)
    runHsc hsc_env $ do
    e <- hscIncrementalFrontend always_do_basic_recompilation_check m_tc_result mHscMessage
            mod_summary source_modified mb_old_iface mod_index
    case e of
        -- We didn't need to do any typechecking; the old interface
        -- file on disk was good enough.
        Left iface -> do
            details <- liftIO $ initModDetails hsc_env mod_summary iface
            return (HscUpToDate iface details, hsc_env')
        -- We finished type checking.  (mb_old_hash is the hash of
        -- the interface that existed on disk; it's possible we had
        -- to retypecheck but the resulting interface is exactly
        -- the same.)
        Right (FrontendTypecheck tc_result, mb_old_hash) -> do
            status <- finish mod_summary tc_result mb_old_hash
            return (status, hsc_env)

-- Knot tying!  See Note [Knot-tying typecheckIface]
-- See Note [ModDetails and --make mode]
initModDetails :: HscEnv -> ModSummary -> ModIface -> IO ModDetails
initModDetails hsc_env mod_summary iface =
  fixIO $ \details' -> do
    let hsc_env' =
          hsc_env {
              hsc_HPT = addToHpt (hsc_HPT hsc_env)
                          (ms_mod_name mod_summary)
                          (HomeModInfo iface details' Nothing)
                  }
    -- NB: This result is actually not that useful
    -- in one-shot mode, since we're not going to do
    -- any further typechecking.  It's much more useful
    -- in make mode, since this HMI will go into the HPT.
    genModDetails hsc_env' iface


{-
Note [ModDetails and --make mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An interface file consists of two parts

* The `ModIface` which ends up getting written to disk.
  The `ModIface` is a completely acyclic tree, which can be serialised
  and de-serialised completely straightforwardly.  The `ModIface` is
  also the structure that is finger-printed for recompilation control.

* The `ModDetails` which provides a more structured view that is suitable
  for usage during compilation.  The `ModDetails` is heavily cyclic:
  An `Id` contains a `Type`, which mentions a `TyCon` that contains kind
  that mentions other `TyCons`; the `Id` also includes an unfolding that
  in turn mentions more `Id`s;  And so on.

The `ModIface` can be created from the `ModDetails` and the `ModDetails` from
a `ModIface`.

During tidying, just before interfaces are written to disk,
the ModDetails is calculated and then converted into a ModIface (see GHC.Iface.Make.mkIface_).
Then when GHC needs to restart typechecking from a certain point it can read the
interface file, and regenerate the ModDetails from the ModIface (see GHC.IfaceToCore.typecheckIface).
The key part about the loading is that the ModDetails is regenerated lazily
from the ModIface, so that there's only a detailed in-memory representation
for declarations which are actually used from the interface. This mode is
also used when reading interface files from external packages.

In the old --make mode implementation, the interface was written after compiling a module
but the in-memory ModDetails which was used to compute the ModIface was retained.
The result was that --make mode used much more memory than `-c` mode, because a large amount of
information about a module would be kept in the ModDetails but never used.

The new idea is that even in `--make` mode, when there is an in-memory `ModDetails`
at hand, we re-create the `ModDetails` from the `ModIface`. Doing this means that
we only have to keep the `ModIface` decls in memory and then lazily load
detailed representations if needed. It turns out this makes a really big difference
to memory usage, halving maximum memory used in some cases.

See !5492 and #13586
-}

-- Runs the post-typechecking frontend (desugar and simplify). We want to
-- generate most of the interface as late as possible. This gets us up-to-date
-- and good unfoldings and other info in the interface file.
--
-- We might create a interface right away, in which case we also return the
-- updated HomeModInfo. But we might also need to run the backend first. In the
-- later case Status will be HscRecomp and we return a function from ModIface ->
-- HomeModInfo.
--
-- HscRecomp in turn will carry the information required to compute a interface
-- when passed the result of the code generator. So all this can and is done at
-- the call site of the backend code gen if it is run.
finish :: ModSummary
       -> TcGblEnv
       -> Maybe Fingerprint
       -> Hsc HscStatus
finish summary tc_result mb_old_hash = do
  hsc_env <- getHscEnv
  dflags <- getDynFlags
  logger <- getLogger
  let bcknd  = backend dflags
      hsc_src = ms_hsc_src summary

  -- Desugar, if appropriate
  --
  -- We usually desugar even when we are not generating code, otherwise we
  -- would miss errors thrown by the desugaring (see #10600). The only
  -- exceptions are when the Module is Ghc.Prim or when it is not a
  -- HsSrcFile Module.
  mb_desugar <-
      if ms_mod summary /= gHC_PRIM && hsc_src == HsSrcFile
      then Just <$> hscDesugar' (ms_location summary) tc_result
      else pure Nothing

  -- Simplify, if appropriate, and (whether we simplified or not) generate an
  -- interface file.
  case mb_desugar of
      -- Just cause we desugared doesn't mean we are generating code, see above.
      Just desugared_guts | bcknd /= NoBackend -> do
          plugins <- liftIO $ readIORef (tcg_th_coreplugins tc_result)
          simplified_guts <- hscSimplify' plugins desugared_guts

          (cg_guts, details) <- {-# SCC "CoreTidy" #-}
              liftIO $ tidyProgram hsc_env simplified_guts

          let !partial_iface =
                {-# SCC "GHC.Driver.Main.mkPartialIface" #-}
                -- This `force` saves 2M residency in test T10370
                -- See Note [Avoiding space leaks in toIface*] for details.
                force (mkPartialIface hsc_env details simplified_guts)

          return HscRecomp { hscs_guts = cg_guts,
                             hscs_mod_location = ms_location summary,
                             hscs_partial_iface = partial_iface,
                             hscs_old_iface_hash = mb_old_hash
                           }

      -- We are not generating code, so we can skip simplification
      -- and generate a simple interface.
      _ -> do
        (iface, mb_old_iface_hash, details) <- liftIO $
          hscSimpleIface hsc_env tc_result mb_old_hash

        liftIO $ hscMaybeWriteIface logger dflags True iface mb_old_iface_hash (ms_location summary)

        return $ case bcknd of
          NoBackend -> HscNotGeneratingCode iface details
          _         -> case hsc_src of
                        HsBootFile -> HscUpdateBoot iface details
                        HsigFile   -> HscUpdateSig iface details
                        _          -> panic "finish"

{-
Note [Writing interface files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We write one interface file per module and per compilation, except with
-dynamic-too where we write two interface files (non-dynamic and dynamic).

We can write two kinds of interfaces (see Note [Interface file stages] in
"GHC.Driver.Types"):

   * simple interface: interface generated after the core pipeline

   * full interface: simple interface completed with information from the
     backend

Depending on the situation, we write one or the other (using
`hscMaybeWriteIface`). We must be careful with `-dynamic-too` because only the
backend is run twice, so if we write a simple interface we need to write both
the non-dynamic and the dynamic interfaces at the same time (with the same
contents).

Cases for which we generate simple interfaces:

   * GHC.Driver.Main.finish: when a compilation does NOT require (re)compilation
   of the hard code

   * GHC.Driver.Pipeline.compileOne': when we run in One Shot mode and target
   bytecode (if interface writing is forced).

   * GHC.Driver.Backpack uses simple interfaces for indefinite units
   (units with module holes). It writes them indirectly by forcing the
   -fwrite-interface flag while setting backend to NoBackend.

Cases for which we generate full interfaces:

   * GHC.Driver.Pipeline.runPhase: when we must be compiling to regular hard
   code and/or require recompilation.

By default interface file names are derived from module file names by adding
suffixes. The interface file name can be overloaded with "-ohi", except when
`-dynamic-too` is used.

-}

-- | Write interface files
hscMaybeWriteIface :: Logger -> DynFlags -> Bool -> ModIface -> Maybe Fingerprint -> ModLocation -> IO ()
hscMaybeWriteIface logger dflags is_simple iface old_iface mod_location = do
    let force_write_interface = gopt Opt_WriteInterface dflags
        write_interface = case backend dflags of
                            NoBackend    -> False
                            Interpreter  -> False
                            _            -> True

      -- mod_location only contains the base name, so we rebuild the
      -- correct file extension from the dynflags.
        baseName = ml_hi_file mod_location
        buildIfName suffix is_dynamic
          | Just name <- (if is_dynamic then dynOutputHi else outputHi) dflags
          = name
          | otherwise
          = let with_hi = replaceExtension baseName suffix
            in  addBootSuffix_maybe (mi_boot iface) with_hi

        write_iface dflags' iface =
          let !iface_name = buildIfName (hiSuf dflags') (dynamicNow dflags')
          in
          {-# SCC "writeIface" #-}
          withTiming logger dflags'
              (text "WriteIface"<+>brackets (text iface_name))
              (const ())
              (writeIface logger dflags' iface_name iface)

    when (write_interface || force_write_interface) $ do

      -- FIXME: with -dynamic-too, "no_change" is only meaningful for the
      -- non-dynamic interface, not for the dynamic one. We should have another
      -- flag for the dynamic interface. In the meantime:
      --
      --    * when we write a single full interface, we check if we are
      --    currently writing the dynamic interface due to -dynamic-too, in
      --    which case we ignore "no_change".
      --
      --    * when we write two simple interfaces at once because of
      --    dynamic-too, we use "no_change" both for the non-dynamic and the
      --    dynamic interfaces. Hopefully both the dynamic and the non-dynamic
      --    interfaces stay in sync...
      --
      let no_change = old_iface == Just (mi_iface_hash (mi_final_exts iface))

      dt <- dynamicTooState dflags

      when (dopt Opt_D_dump_if_trace dflags) $ putMsg logger dflags $
        hang (text "Writing interface(s):") 2 $ vcat
         [ text "Kind:" <+> if is_simple then text "simple" else text "full"
         , text "Hash change:" <+> ppr (not no_change)
         , text "DynamicToo state:" <+> text (show dt)
         ]

      if is_simple
         then unless no_change $ do -- FIXME: see no_change' comment above
            write_iface dflags iface
            case dt of
               DT_Dont   -> return ()
               DT_Failed -> return ()
               DT_Dyn    -> panic "Unexpected DT_Dyn state when writing simple interface"
               DT_OK     -> write_iface (setDynamicNow dflags) iface
         else case dt of
               DT_Dont | not no_change             -> write_iface dflags iface
               DT_OK   | not no_change             -> write_iface dflags iface
               -- FIXME: see no_change' comment above
               DT_Dyn                              -> write_iface dflags iface
               DT_Failed | not (dynamicNow dflags) -> write_iface dflags iface
               _                                   -> return ()

--------------------------------------------------------------
-- NoRecomp handlers
--------------------------------------------------------------

-- NB: this must be knot-tied appropriately, see hscIncrementalCompile
genModDetails :: HscEnv -> ModIface -> IO ModDetails
genModDetails hsc_env old_iface
  = do
    new_details <- {-# SCC "tcRnIface" #-}
                   initIfaceLoad hsc_env (typecheckIface old_iface)
    dumpIfaceStats hsc_env
    return new_details

--------------------------------------------------------------
-- Progress displayers.
--------------------------------------------------------------

oneShotMsg :: HscEnv -> RecompileRequired -> IO ()
oneShotMsg hsc_env recomp =
    case recomp of
        UpToDate ->
            compilationProgressMsg logger dflags $
                   text "compilation IS NOT required"
        _ ->
            return ()
    where
        dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env

batchMsg :: Messager
batchMsg hsc_env mod_index recomp node = case node of
    InstantiationNode _ ->
        case recomp of
            MustCompile -> showMsg (text "Instantiating ") empty
            UpToDate
                | verbosity dflags >= 2 -> showMsg (text "Skipping  ") empty
                | otherwise -> return ()
            RecompBecause reason -> showMsg (text "Instantiating ") (text " [" <> text reason <> text "]")
    ModuleNode _ ->
        case recomp of
            MustCompile -> showMsg (text "Compiling ") empty
            UpToDate
                | verbosity dflags >= 2 -> showMsg (text "Skipping  ") empty
                | otherwise -> return ()
            RecompBecause reason -> showMsg (text "Compiling ") (text " [" <> text reason <> text "]")
    where
        dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env
        showMsg msg reason =
            compilationProgressMsg logger dflags $
            (showModuleIndex mod_index <>
            msg <> showModMsg dflags (recompileRequired recomp) node)
                <> reason

--------------------------------------------------------------
-- Safe Haskell
--------------------------------------------------------------

-- Note [Safe Haskell Trust Check]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Safe Haskell checks that an import is trusted according to the following
-- rules for an import of module M that resides in Package P:
--
--   * If M is recorded as Safe and all its trust dependencies are OK
--     then M is considered safe.
--   * If M is recorded as Trustworthy and P is considered trusted and
--     all M's trust dependencies are OK then M is considered safe.
--
-- By trust dependencies we mean that the check is transitive. So if
-- a module M that is Safe relies on a module N that is trustworthy,
-- importing module M will first check (according to the second case)
-- that N is trusted before checking M is trusted.
--
-- This is a minimal description, so please refer to the user guide
-- for more details. The user guide is also considered the authoritative
-- source in this matter, not the comments or code.


-- Note [Safe Haskell Inference]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Safe Haskell does Safe inference on modules that don't have any specific
-- safe haskell mode flag. The basic approach to this is:
--   * When deciding if we need to do a Safe language check, treat
--     an unmarked module as having -XSafe mode specified.
--   * For checks, don't throw errors but return them to the caller.
--   * Caller checks if there are errors:
--     * For modules explicitly marked -XSafe, we throw the errors.
--     * For unmarked modules (inference mode), we drop the errors
--       and mark the module as being Unsafe.
--
-- It used to be that we only did safe inference on modules that had no Safe
-- Haskell flags, but now we perform safe inference on all modules as we want
-- to allow users to set the `-Wsafe`, `-Wunsafe` and
-- `-Wtrustworthy-safe` flags on Trustworthy and Unsafe modules so that a
-- user can ensure their assumptions are correct and see reasons for why a
-- module is safe or unsafe.
--
-- This is tricky as we must be careful when we should throw an error compared
-- to just warnings. For checking safe imports we manage it as two steps. First
-- we check any imports that are required to be safe, then we check all other
-- imports to see if we can infer them to be safe.


-- | Check that the safe imports of the module being compiled are valid.
-- If not we either issue a compilation error if the module is explicitly
-- using Safe Haskell, or mark the module as unsafe if we're in safe
-- inference mode.
hscCheckSafeImports :: TcGblEnv -> Hsc TcGblEnv
hscCheckSafeImports tcg_env = do
    dflags   <- getDynFlags
    tcg_env' <- checkSafeImports tcg_env
    checkRULES dflags tcg_env'

  where
    checkRULES dflags tcg_env' =
      case safeLanguageOn dflags of
          True -> do
              -- XSafe: we nuke user written RULES
              logWarnings $ warns (tcg_rules tcg_env')
              return tcg_env' { tcg_rules = [] }
          False
                -- SafeInferred: user defined RULES, so not safe
              | safeInferOn dflags && not (null $ tcg_rules tcg_env')
              -> markUnsafeInfer tcg_env' $ warns (tcg_rules tcg_env')

                -- Trustworthy OR SafeInferred: with no RULES
              | otherwise
              -> return tcg_env'

    warns rules = listToBag $ map warnRules rules

    warnRules :: LRuleDecl GhcTc -> MsgEnvelope DecoratedSDoc
    warnRules (L loc (HsRule { rd_name = n })) =
        mkPlainWarnMsg (locA loc) $
            text "Rule \"" <> ftext (snd $ unLoc n) <> text "\" ignored" $+$
            text "User defined rules are disabled under Safe Haskell"

-- | Validate that safe imported modules are actually safe.  For modules in the
-- HomePackage (the package the module we are compiling in resides) this just
-- involves checking its trust type is 'Safe' or 'Trustworthy'. For modules
-- that reside in another package we also must check that the external package
-- is trusted. See the Note [Safe Haskell Trust Check] above for more
-- information.
--
-- The code for this is quite tricky as the whole algorithm is done in a few
-- distinct phases in different parts of the code base. See
-- 'GHC.Rename.Names.rnImportDecl' for where package trust dependencies for a
-- module are collected and unioned.  Specifically see the Note [Tracking Trust
-- Transitively] in "GHC.Rename.Names" and the Note [Trust Own Package] in
-- "GHC.Rename.Names".
checkSafeImports :: TcGblEnv -> Hsc TcGblEnv
checkSafeImports tcg_env
    = do
        dflags <- getDynFlags
        imps <- mapM condense imports'
        let (safeImps, regImps) = partition (\(_,_,s) -> s) imps

        -- We want to use the warning state specifically for detecting if safe
        -- inference has failed, so store and clear any existing warnings.
        oldErrs <- getWarnings
        clearWarnings

        -- Check safe imports are correct
        safePkgs <- S.fromList <$> mapMaybeM checkSafe safeImps
        safeErrs <- getWarnings
        clearWarnings

        -- Check non-safe imports are correct if inferring safety
        -- See the Note [Safe Haskell Inference]
        (infErrs, infPkgs) <- case (safeInferOn dflags) of
          False -> return (emptyBag, S.empty)
          True -> do infPkgs <- S.fromList <$> mapMaybeM checkSafe regImps
                     infErrs <- getWarnings
                     clearWarnings
                     return (infErrs, infPkgs)

        -- restore old errors
        logWarnings oldErrs

        case (isEmptyBag safeErrs) of
          -- Failed safe check
          False -> liftIO . throwIO . mkSrcErr $ safeErrs

          -- Passed safe check
          True -> do
            let infPassed = isEmptyBag infErrs
            tcg_env' <- case (not infPassed) of
              True  -> markUnsafeInfer tcg_env infErrs
              False -> return tcg_env
            when (packageTrustOn dflags) $ checkPkgTrust pkgReqs
            let newTrust = pkgTrustReqs dflags safePkgs infPkgs infPassed
            return tcg_env' { tcg_imports = impInfo `plusImportAvails` newTrust }

  where
    impInfo  = tcg_imports tcg_env     -- ImportAvails
    imports  = imp_mods impInfo        -- ImportedMods
    imports1 = moduleEnvToList imports -- (Module, [ImportedBy])
    imports' = map (fmap importedByUser) imports1 -- (Module, [ImportedModsVal])
    pkgReqs  = imp_trust_pkgs impInfo  -- [Unit]

    condense :: (Module, [ImportedModsVal]) -> Hsc (Module, SrcSpan, IsSafeImport)
    condense (_, [])   = panic "GHC.Driver.Main.condense: Pattern match failure!"
    condense (m, x:xs) = do imv <- foldlM cond' x xs
                            return (m, imv_span imv, imv_is_safe imv)

    -- ImportedModsVal = (ModuleName, Bool, SrcSpan, IsSafeImport)
    cond' :: ImportedModsVal -> ImportedModsVal -> Hsc ImportedModsVal
    cond' v1 v2
        | imv_is_safe v1 /= imv_is_safe v2
        = throwOneError $ mkPlainMsgEnvelope (imv_span v1)
              (text "Module" <+> ppr (imv_name v1) <+>
              (text $ "is imported both as a safe and unsafe import!"))
        | otherwise
        = return v1

    -- easier interface to work with
    checkSafe :: (Module, SrcSpan, a) -> Hsc (Maybe UnitId)
    checkSafe (m, l, _) = fst `fmap` hscCheckSafe' m l

    -- what pkg's to add to our trust requirements
    pkgTrustReqs :: DynFlags -> Set UnitId -> Set UnitId ->
          Bool -> ImportAvails
    pkgTrustReqs dflags req inf infPassed | safeInferOn dflags
                                  && not (safeHaskellModeEnabled dflags) && infPassed
                                   = emptyImportAvails {
                                       imp_trust_pkgs = req `S.union` inf
                                   }
    pkgTrustReqs dflags _   _ _ | safeHaskell dflags == Sf_Unsafe
                         = emptyImportAvails
    pkgTrustReqs _ req _ _ = emptyImportAvails { imp_trust_pkgs = req }

-- | Check that a module is safe to import.
--
-- We return True to indicate the import is safe and False otherwise
-- although in the False case an exception may be thrown first.
hscCheckSafe :: HscEnv -> Module -> SrcSpan -> IO Bool
hscCheckSafe hsc_env m l = runHsc hsc_env $ do
    dflags <- getDynFlags
    pkgs <- snd `fmap` hscCheckSafe' m l
    when (packageTrustOn dflags) $ checkPkgTrust pkgs
    errs <- getWarnings
    return $ isEmptyBag errs

-- | Return if a module is trusted and the pkgs it depends on to be trusted.
hscGetSafe :: HscEnv -> Module -> SrcSpan -> IO (Bool, Set UnitId)
hscGetSafe hsc_env m l = runHsc hsc_env $ do
    (self, pkgs) <- hscCheckSafe' m l
    good         <- isEmptyBag `fmap` getWarnings
    clearWarnings -- don't want them printed...
    let pkgs' | Just p <- self = S.insert p pkgs
              | otherwise      = pkgs
    return (good, pkgs')

-- | Is a module trusted? If not, throw or log errors depending on the type.
-- Return (regardless of trusted or not) if the trust type requires the modules
-- own package be trusted and a list of other packages required to be trusted
-- (these later ones haven't been checked) but the own package trust has been.
hscCheckSafe' :: Module -> SrcSpan
  -> Hsc (Maybe UnitId, Set UnitId)
hscCheckSafe' m l = do
    hsc_env <- getHscEnv
    let home_unit = hsc_home_unit hsc_env
    (tw, pkgs) <- isModSafe home_unit m l
    case tw of
        False                           -> return (Nothing, pkgs)
        True | isHomeModule home_unit m -> return (Nothing, pkgs)
             -- TODO: do we also have to check the trust of the instantiation?
             -- Not necessary if that is reflected in dependencies
             | otherwise   -> return (Just $ toUnitId (moduleUnit m), pkgs)
  where
    isModSafe :: HomeUnit -> Module -> SrcSpan -> Hsc (Bool, Set UnitId)
    isModSafe home_unit m l = do
        hsc_env <- getHscEnv
        dflags <- getDynFlags
        iface <- lookup' m
        case iface of
            -- can't load iface to check trust!
            Nothing -> throwOneError $ mkPlainMsgEnvelope l
                         $ text "Can't load the interface file for" <+> ppr m
                           <> text ", to check that it can be safely imported"

            -- got iface, check trust
            Just iface' ->
                let trust = getSafeMode $ mi_trust iface'
                    trust_own_pkg = mi_trust_pkg iface'
                    -- check module is trusted
                    safeM = trust `elem` [Sf_Safe, Sf_SafeInferred, Sf_Trustworthy]
                    -- check package is trusted
                    safeP = packageTrusted dflags (hsc_units hsc_env) home_unit trust trust_own_pkg m
                    -- pkg trust reqs
                    pkgRs = S.fromList . map fst $ filter snd $ dep_pkgs $ mi_deps iface'
                    -- warn if Safe module imports Safe-Inferred module.
                    warns = if wopt Opt_WarnInferredSafeImports dflags
                                && safeLanguageOn dflags
                                && trust == Sf_SafeInferred
                                then inferredImportWarn
                                else emptyBag
                    -- General errors we throw but Safe errors we log
                    errs = case (safeM, safeP) of
                        (True, True ) -> emptyBag
                        (True, False) -> pkgTrustErr
                        (False, _   ) -> modTrustErr
                in do
                    logWarnings warns
                    logWarnings errs
                    return (trust == Sf_Trustworthy, pkgRs)

                where
                    state = hsc_units hsc_env
                    inferredImportWarn = unitBag
                        $ makeIntoWarning (Reason Opt_WarnInferredSafeImports)
                        $ mkWarnMsg l (pkgQual state)
                        $ sep
                            [ text "Importing Safe-Inferred module "
                                <> ppr (moduleName m)
                                <> text " from explicitly Safe module"
                            ]
                    pkgTrustErr = unitBag $ mkMsgEnvelope l (pkgQual state) $
                        sep [ ppr (moduleName m)
                                <> text ": Can't be safely imported!"
                            , text "The package ("
                                <> (pprWithUnitState state $ ppr (moduleUnit m))
                                <> text ") the module resides in isn't trusted."
                            ]
                    modTrustErr = unitBag $ mkMsgEnvelope l (pkgQual state) $
                        sep [ ppr (moduleName m)
                                <> text ": Can't be safely imported!"
                            , text "The module itself isn't safe." ]

    -- | Check the package a module resides in is trusted. Safe compiled
    -- modules are trusted without requiring that their package is trusted. For
    -- trustworthy modules, modules in the home package are trusted but
    -- otherwise we check the package trust flag.
    packageTrusted :: DynFlags -> UnitState -> HomeUnit -> SafeHaskellMode -> Bool -> Module -> Bool
    packageTrusted dflags unit_state home_unit safe_mode trust_own_pkg mod =
        case safe_mode of
            Sf_None      -> False -- shouldn't hit these cases
            Sf_Ignore    -> False -- shouldn't hit these cases
            Sf_Unsafe    -> False -- prefer for completeness.
            _ | not (packageTrustOn dflags)     -> True
            Sf_Safe | not trust_own_pkg         -> True
            Sf_SafeInferred | not trust_own_pkg -> True
            _ | isHomeModule home_unit mod      -> True
            _ -> unitIsTrusted $ unsafeLookupUnit unit_state (moduleUnit m)

    lookup' :: Module -> Hsc (Maybe ModIface)
    lookup' m = do
        hsc_env <- getHscEnv
        hsc_eps <- liftIO $ hscEPS hsc_env
        let pkgIfaceT = eps_PIT hsc_eps
            homePkgT  = hsc_HPT hsc_env
            iface     = lookupIfaceByModule homePkgT pkgIfaceT m
        -- the 'lookupIfaceByModule' method will always fail when calling from GHCi
        -- as the compiler hasn't filled in the various module tables
        -- so we need to call 'getModuleInterface' to load from disk
        case iface of
            Just _  -> return iface
            Nothing -> snd `fmap` (liftIO $ getModuleInterface hsc_env m)


-- | Check the list of packages are trusted.
checkPkgTrust :: Set UnitId -> Hsc ()
checkPkgTrust pkgs = do
    hsc_env <- getHscEnv
    let errors = S.foldr go [] pkgs
        state  = hsc_units hsc_env
        go pkg acc
            | unitIsTrusted $ unsafeLookupUnitId state pkg
            = acc
            | otherwise
            = (:acc) $ mkMsgEnvelope noSrcSpan (pkgQual state)
                     $ pprWithUnitState state
                     $ text "The package ("
                        <> ppr pkg
                        <> text ") is required to be trusted but it isn't!"
    case errors of
        [] -> return ()
        _  -> (liftIO . throwIO . mkSrcErr . listToBag) errors

-- | Set module to unsafe and (potentially) wipe trust information.
--
-- Make sure to call this method to set a module to inferred unsafe, it should
-- be a central and single failure method. We only wipe the trust information
-- when we aren't in a specific Safe Haskell mode.
--
-- While we only use this for recording that a module was inferred unsafe, we
-- may call it on modules using Trustworthy or Unsafe flags so as to allow
-- warning flags for safety to function correctly. See Note [Safe Haskell
-- Inference].
markUnsafeInfer :: TcGblEnv -> WarningMessages -> Hsc TcGblEnv
markUnsafeInfer tcg_env whyUnsafe = do
    dflags <- getDynFlags

    when (wopt Opt_WarnUnsafe dflags)
         (logWarnings $ unitBag $ makeIntoWarning (Reason Opt_WarnUnsafe) $
             mkPlainWarnMsg (warnUnsafeOnLoc dflags) (whyUnsafe' dflags))

    liftIO $ writeIORef (tcg_safeInfer tcg_env) (False, whyUnsafe)
    -- NOTE: Only wipe trust when not in an explicitly safe haskell mode. Other
    -- times inference may be on but we are in Trustworthy mode -- so we want
    -- to record safe-inference failed but not wipe the trust dependencies.
    case not (safeHaskellModeEnabled dflags) of
      True  -> return $ tcg_env { tcg_imports = wiped_trust }
      False -> return tcg_env

  where
    wiped_trust   = (tcg_imports tcg_env) { imp_trust_pkgs = S.empty }
    pprMod        = ppr $ moduleName $ tcg_mod tcg_env
    whyUnsafe' df = vcat [ quotes pprMod <+> text "has been inferred as unsafe!"
                         , text "Reason:"
                         , nest 4 $ (vcat $ badFlags df) $+$
                                    (vcat $ pprMsgEnvelopeBagWithLoc whyUnsafe) $+$
                                    (vcat $ badInsts $ tcg_insts tcg_env)
                         ]
    badFlags df   = concatMap (badFlag df) unsafeFlagsForInfer
    badFlag df (str,loc,on,_)
        | on df     = [mkLocMessage SevOutput (loc df) $
                            text str <+> text "is not allowed in Safe Haskell"]
        | otherwise = []
    badInsts insts = concatMap badInst insts

    checkOverlap (NoOverlap _) = False
    checkOverlap _             = True

    badInst ins | checkOverlap (overlapMode (is_flag ins))
                = [mkLocMessage SevOutput (nameSrcSpan $ getName $ is_dfun ins) $
                      ppr (overlapMode $ is_flag ins) <+>
                      text "overlap mode isn't allowed in Safe Haskell"]
                | otherwise = []


-- | Figure out the final correct safe haskell mode
hscGetSafeMode :: TcGblEnv -> Hsc SafeHaskellMode
hscGetSafeMode tcg_env = do
    dflags  <- getDynFlags
    liftIO $ finalSafeMode dflags tcg_env

--------------------------------------------------------------
-- Simplifiers
--------------------------------------------------------------

-- | Run Core2Core simplifier. The list of String is a list of (Core) plugin
-- module names added via TH (cf 'addCorePlugin').
hscSimplify :: HscEnv -> [String] -> ModGuts -> IO ModGuts
hscSimplify hsc_env plugins modguts =
    runHsc hsc_env $ hscSimplify' plugins modguts

-- | Run Core2Core simplifier. The list of String is a list of (Core) plugin
-- module names added via TH (cf 'addCorePlugin').
hscSimplify' :: [String] -> ModGuts -> Hsc ModGuts
hscSimplify' plugins ds_result = do
    hsc_env <- getHscEnv
    hsc_env_with_plugins <- if null plugins -- fast path
        then return hsc_env
        else liftIO $ initializePlugins $ hsc_env
                { hsc_dflags = foldr addPluginModuleName (hsc_dflags hsc_env) plugins
                }
    {-# SCC "Core2Core" #-}
      liftIO $ core2core hsc_env_with_plugins ds_result

--------------------------------------------------------------
-- Interface generators
--------------------------------------------------------------

-- | Generate a striped down interface file, e.g. for boot files or when ghci
-- generates interface files. See Note [simpleTidyPgm - mkBootModDetailsTc]
hscSimpleIface :: HscEnv
               -> TcGblEnv
               -> Maybe Fingerprint
               -> IO (ModIface, Maybe Fingerprint, ModDetails)
hscSimpleIface hsc_env tc_result mb_old_iface
    = runHsc hsc_env $ hscSimpleIface' tc_result mb_old_iface

hscSimpleIface' :: TcGblEnv
                -> Maybe Fingerprint
                -> Hsc (ModIface, Maybe Fingerprint, ModDetails)
hscSimpleIface' tc_result mb_old_iface = do
    hsc_env   <- getHscEnv
    details   <- liftIO $ mkBootModDetailsTc hsc_env tc_result
    safe_mode <- hscGetSafeMode tc_result
    new_iface
        <- {-# SCC "MkFinalIface" #-}
           liftIO $
               mkIfaceTc hsc_env safe_mode details tc_result
    -- And the answer is ...
    liftIO $ dumpIfaceStats hsc_env
    return (new_iface, mb_old_iface, details)

--------------------------------------------------------------
-- BackEnd combinators
--------------------------------------------------------------

-- | Compile to hard-code.
hscGenHardCode :: HscEnv -> CgGuts -> ModLocation -> FilePath
               -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)], CgInfos)
               -- ^ @Just f@ <=> _stub.c is f
hscGenHardCode hsc_env cgguts location output_filename = do
        let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                    -- From now on, we just use the bits we need.
                    cg_module   = this_mod,
                    cg_binds    = core_binds,
                    cg_tycons   = tycons,
                    cg_foreign  = foreign_stubs0,
                    cg_foreign_files = foreign_files,
                    cg_dep_pkgs = dependencies,
                    cg_hpc_info = hpc_info } = cgguts
            dflags = hsc_dflags hsc_env
            logger = hsc_logger hsc_env
            hooks  = hsc_hooks hsc_env
            tmpfs  = hsc_tmpfs hsc_env
            data_tycons = filter isDataTyCon tycons
            -- cg_tycons includes newtypes, for the benefit of External Core,
            -- but we don't generate any code for newtypes

        -------------------
        -- PREPARE FOR CODE GENERATION
        -- Do saturation and convert to A-normal form
        (prepd_binds, local_ccs) <- {-# SCC "CorePrep" #-}
                       corePrepPgm hsc_env this_mod location
                                   core_binds data_tycons

        -----------------  Convert to STG ------------------
        (stg_binds, denv, (caf_ccs, caf_cc_stacks))
            <- {-# SCC "CoreToStg" #-}
               withTiming logger dflags
                   (text "CoreToStg"<+>brackets (ppr this_mod))
                   (\(a, b, (c,d)) -> a `seqList` b `seq` c `seqList` d `seqList` ())
                   (myCoreToStg logger dflags (hsc_IC hsc_env) this_mod location prepd_binds)

        let cost_centre_info =
              (S.toList local_ccs ++ caf_ccs, caf_cc_stacks)
            platform = targetPlatform dflags
            prof_init
              | sccProfilingEnabled dflags = profilingInitCode platform this_mod cost_centre_info
              | otherwise = mempty

        ------------------  Code generation ------------------
        -- The back-end is streamed: each top-level function goes
        -- from Stg all the way to asm before dealing with the next
        -- top-level function, so showPass isn't very useful here.
        -- Hence we have one showPass for the whole backend, the
        -- next showPass after this will be "Assembler".
        withTiming logger dflags
                   (text "CodeGen"<+>brackets (ppr this_mod))
                   (const ()) $ do
            cmms <- {-# SCC "StgToCmm" #-}
                            doCodeGen hsc_env this_mod denv data_tycons
                                cost_centre_info
                                stg_binds hpc_info

            ------------------  Code output -----------------------
            rawcmms0 <- {-# SCC "cmmToRawCmm" #-}
                        case cmmToRawCmmHook hooks of
                            Nothing -> cmmToRawCmm logger dflags cmms
                            Just h  -> h dflags (Just this_mod) cmms

            let dump a = do
                  unless (null a) $
                    dumpIfSet_dyn logger dflags Opt_D_dump_cmm_raw "Raw Cmm" FormatCMM (pdoc platform a)
                  return a
                rawcmms1 = Stream.mapM dump rawcmms0

            let foreign_stubs st = foreign_stubs0 `appendStubC` prof_init
                                                  `appendStubC` cgIPEStub st

            (output_filename, (_stub_h_exists, stub_c_exists), foreign_fps, cg_infos)
                <- {-# SCC "codeOutput" #-}
                  codeOutput logger tmpfs dflags (hsc_units hsc_env) this_mod output_filename location
                  foreign_stubs foreign_files dependencies rawcmms1
            return (output_filename, stub_c_exists, foreign_fps, cg_infos)


hscInteractive :: HscEnv
               -> CgGuts
               -> ModLocation
               -> IO (Maybe FilePath, CompiledByteCode, [SptEntry])
hscInteractive hsc_env cgguts location = do
    let dflags = hsc_dflags hsc_env
    let logger = hsc_logger hsc_env
    let tmpfs  = hsc_tmpfs hsc_env
    let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                -- From now on, we just use the bits we need.
               cg_module   = this_mod,
               cg_binds    = core_binds,
               cg_tycons   = tycons,
               cg_foreign  = foreign_stubs,
               cg_modBreaks = mod_breaks,
               cg_spt_entries = spt_entries } = cgguts

        data_tycons = filter isDataTyCon tycons
        -- cg_tycons includes newtypes, for the benefit of External Core,
        -- but we don't generate any code for newtypes

    -------------------
    -- PREPARE FOR CODE GENERATION
    -- Do saturation and convert to A-normal form
    (prepd_binds, _) <- {-# SCC "CorePrep" #-}
                   corePrepPgm hsc_env this_mod location core_binds data_tycons

    (stg_binds, _infotable_prov, _caf_ccs__caf_cc_stacks)
      <- {-# SCC "CoreToStg" #-}
          myCoreToStg logger dflags (hsc_IC hsc_env) this_mod location prepd_binds
    -----------------  Generate byte code ------------------
    comp_bc <- byteCodeGen hsc_env this_mod stg_binds data_tycons mod_breaks
    ------------------ Create f-x-dynamic C-side stuff -----
    (_istub_h_exists, istub_c_exists)
        <- outputForeignStubs logger tmpfs dflags (hsc_units hsc_env) this_mod location foreign_stubs
    return (istub_c_exists, comp_bc, spt_entries)

------------------------------

hscCompileCmmFile :: HscEnv -> FilePath -> FilePath -> IO (Maybe FilePath)
hscCompileCmmFile hsc_env filename output_filename = runHsc hsc_env $ do
    let dflags   = hsc_dflags hsc_env
    let logger   = hsc_logger hsc_env
    let hooks    = hsc_hooks hsc_env
    let tmpfs    = hsc_tmpfs hsc_env
        home_unit = hsc_home_unit hsc_env
        platform  = targetPlatform dflags
        -- Make up a module name to give the NCG. We can't pass bottom here
        -- lest we reproduce #11784.
        mod_name = mkModuleName $ "Cmm$" ++ FilePath.takeFileName filename
        cmm_mod = mkHomeModule home_unit mod_name
    (cmm, ents) <- ioMsgMaybe
               $ do
                  (warns,errs,cmm) <- withTiming logger dflags (text "ParseCmm"<+>brackets (text filename)) (\_ -> ())
                                       $ parseCmmFile dflags cmm_mod home_unit filename
                  return (mkMessages (fmap pprWarning warns `unionBags` fmap pprError errs), cmm)
    liftIO $ do
        dumpIfSet_dyn logger dflags Opt_D_dump_cmm_verbose_by_proc "Parsed Cmm" FormatCMM (pdoc platform cmm)

        -- Compile decls in Cmm files one decl at a time, to avoid re-ordering
        -- them in SRT analysis.
        --
        -- Re-ordering here causes breakage when booting with C backend because
        -- in C we must declare before use, but SRT algorithm is free to
        -- re-order [A, B] (B refers to A) when A is not CAFFY and return [B, A]
        cmmgroup <-
          concatMapM (\cmm -> snd <$> cmmPipeline hsc_env (emptySRT cmm_mod) [cmm]) cmm

        unless (null cmmgroup) $
          dumpIfSet_dyn logger dflags Opt_D_dump_cmm "Output Cmm"
            FormatCMM (pdoc platform cmmgroup)

        rawCmms <- case cmmToRawCmmHook hooks of
          Nothing -> cmmToRawCmm logger dflags         (Stream.yield cmmgroup)
          Just h  -> h                  dflags Nothing (Stream.yield cmmgroup)

        let foreign_stubs _ =
              let ip_init = ipInitCode dflags cmm_mod ents
              in NoStubs `appendStubC` ip_init

        (_output_filename, (_stub_h_exists, stub_c_exists), _foreign_fps, _caf_infos)
          <- codeOutput logger tmpfs dflags (hsc_units hsc_env) cmm_mod output_filename no_loc foreign_stubs [] []
             rawCmms
        return stub_c_exists
  where
    no_loc = ModLocation{ ml_hs_file  = Just filename,
                          ml_hi_file  = panic "hscCompileCmmFile: no hi file",
                          ml_obj_file = panic "hscCompileCmmFile: no obj file",
                          ml_hie_file = panic "hscCompileCmmFile: no hie file"}

-------------------- Stuff for new code gen ---------------------

{-
Note [Forcing of stg_binds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The two last steps in the STG pipeline are:

* Sorting the bindings in dependency order.
* Annotating them with free variables.

We want to make sure we do not keep references to unannotated STG bindings
alive, nor references to bindings which have already been compiled to Cmm.

We explicitly force the bindings to avoid this.

This reduces residency towards the end of the CodeGen phase significantly
(5-10%).
-}

doCodeGen   :: HscEnv -> Module -> InfoTableProvMap -> [TyCon]
            -> CollectedCCs
            -> [StgTopBinding]
            -> HpcInfo
            -> IO (Stream IO CmmGroupSRTs CgInfos)
         -- Note we produce a 'Stream' of CmmGroups, so that the
         -- backend can be run incrementally.  Otherwise it generates all
         -- the C-- up front, which has a significant space cost.
doCodeGen hsc_env this_mod denv data_tycons
              cost_centre_info stg_binds hpc_info = do
    let dflags = hsc_dflags hsc_env
    let logger = hsc_logger hsc_env
    let hooks  = hsc_hooks hsc_env
    let tmpfs  = hsc_tmpfs hsc_env
    let platform = targetPlatform dflags

    let stg_binds_w_fvs = annTopBindingsFreeVars stg_binds

    dumpIfSet_dyn logger dflags Opt_D_dump_stg_final "Final STG:" FormatSTG (pprGenStgTopBindings (initStgPprOpts dflags) stg_binds_w_fvs)

    let stg_to_cmm = case stgToCmmHook hooks of
                        Nothing -> StgToCmm.codeGen logger tmpfs
                        Just h  -> h

    let cmm_stream :: Stream IO CmmGroup (CStub, ModuleLFInfos)
        -- See Note [Forcing of stg_binds]
        cmm_stream = stg_binds_w_fvs `seqList` {-# SCC "StgToCmm" #-}
            stg_to_cmm dflags this_mod denv data_tycons cost_centre_info stg_binds_w_fvs hpc_info

        -- codegen consumes a stream of CmmGroup, and produces a new
        -- stream of CmmGroup (not necessarily synchronised: one
        -- CmmGroup on input may produce many CmmGroups on output due
        -- to proc-point splitting).

    let dump1 a = do
          unless (null a) $
            dumpIfSet_dyn logger dflags Opt_D_dump_cmm_from_stg
              "Cmm produced by codegen" FormatCMM (pdoc platform a)
          return a

        ppr_stream1 = Stream.mapM dump1 cmm_stream

        pipeline_stream :: Stream IO CmmGroupSRTs CgInfos
        pipeline_stream = do
          (non_cafs, (used_info, lf_infos)) <-
            {-# SCC "cmmPipeline" #-}
            Stream.mapAccumL_ (cmmPipeline hsc_env) (emptySRT this_mod) ppr_stream1
              <&> first (srtMapNonCAFs . moduleSRTMap)

          return CgInfos{ cgNonCafs = non_cafs, cgLFInfos = lf_infos, cgIPEStub = used_info }

        dump2 a = do
          unless (null a) $
            dumpIfSet_dyn logger dflags Opt_D_dump_cmm "Output Cmm" FormatCMM (pdoc platform a)
          return a

    return (Stream.mapM dump2 pipeline_stream)

myCoreToStgExpr :: Logger -> DynFlags -> InteractiveContext
                -> Module -> ModLocation -> CoreExpr
                -> IO ( Id
                      , [StgTopBinding]
                      , InfoTableProvMap
                      , CollectedCCs )
myCoreToStgExpr logger dflags ictxt this_mod ml prepd_expr = do
    {- Create a temporary binding (just because myCoreToStg needs a
       binding for the stg2stg step) -}
    let bco_tmp_id = mkSysLocal (fsLit "BCO_toplevel")
                                (mkPseudoUniqueE 0)
                                Many
                                (exprType prepd_expr)
    (stg_binds, prov_map, collected_ccs) <-
       myCoreToStg logger
                   dflags
                   ictxt
                   this_mod
                   ml
                   [NonRec bco_tmp_id prepd_expr]
    return (bco_tmp_id, stg_binds, prov_map, collected_ccs)

myCoreToStg :: Logger -> DynFlags -> InteractiveContext
            -> Module -> ModLocation -> CoreProgram
            -> IO ( [StgTopBinding] -- output program
                  , InfoTableProvMap
                  , CollectedCCs )  -- CAF cost centre info (declared and used)
myCoreToStg logger dflags ictxt this_mod ml prepd_binds = do
    let (stg_binds, denv, cost_centre_info)
         = {-# SCC "Core2Stg" #-}
           coreToStg dflags this_mod ml prepd_binds

    stg_binds2
        <- {-# SCC "Stg2Stg" #-}
           stg2stg logger dflags ictxt this_mod stg_binds

    return (stg_binds2, denv, cost_centre_info)

{- **********************************************************************
%*                                                                      *
\subsection{Compiling a do-statement}
%*                                                                      *
%********************************************************************* -}

{-
When the UnlinkedBCOExpr is linked you get an HValue of type *IO [HValue]* When
you run it you get a list of HValues that should be the same length as the list
of names; add them to the ClosureEnv.

A naked expression returns a singleton Name [it]. The stmt is lifted into the
IO monad as explained in Note [Interactively-bound Ids in GHCi] in GHC.Runtime.Context
-}

-- | Compile a stmt all the way to an HValue, but don't run it
--
-- We return Nothing to indicate an empty statement (or comment only), not a
-- parse error.
hscStmt :: HscEnv -> String -> IO (Maybe ([Id], ForeignHValue, FixityEnv))
hscStmt hsc_env stmt = hscStmtWithLocation hsc_env stmt "<interactive>" 1

-- | Compile a stmt all the way to an HValue, but don't run it
--
-- We return Nothing to indicate an empty statement (or comment only), not a
-- parse error.
hscStmtWithLocation :: HscEnv
                    -> String -- ^ The statement
                    -> String -- ^ The source
                    -> Int    -- ^ Starting line
                    -> IO ( Maybe ([Id]
                          , ForeignHValue {- IO [HValue] -}
                          , FixityEnv))
hscStmtWithLocation hsc_env0 stmt source linenumber =
  runInteractiveHsc hsc_env0 $ do
    maybe_stmt <- hscParseStmtWithLocation source linenumber stmt
    case maybe_stmt of
      Nothing -> return Nothing

      Just parsed_stmt -> do
        hsc_env <- getHscEnv
        liftIO $ hscParsedStmt hsc_env parsed_stmt

hscParsedStmt :: HscEnv
              -> GhciLStmt GhcPs  -- ^ The parsed statement
              -> IO ( Maybe ([Id]
                    , ForeignHValue {- IO [HValue] -}
                    , FixityEnv))
hscParsedStmt hsc_env stmt = runInteractiveHsc hsc_env $ do
  -- Rename and typecheck it
  (ids, tc_expr, fix_env) <- ioMsgMaybe $ tcRnStmt hsc_env stmt

  -- Desugar it
  ds_expr <- ioMsgMaybe $ deSugarExpr hsc_env tc_expr
  liftIO (lintInteractiveExpr (text "desugar expression") hsc_env ds_expr)
  handleWarnings

  -- Then code-gen, and link it
  -- It's important NOT to have package 'interactive' as thisUnitId
  -- for linking, else we try to link 'main' and can't find it.
  -- Whereas the linker already knows to ignore 'interactive'
  let src_span = srcLocSpan interactiveSrcLoc
  hval <- liftIO $ hscCompileCoreExpr hsc_env src_span ds_expr

  return $ Just (ids, hval, fix_env)

-- | Compile a decls
hscDecls :: HscEnv
         -> String -- ^ The statement
         -> IO ([TyThing], InteractiveContext)
hscDecls hsc_env str = hscDeclsWithLocation hsc_env str "<interactive>" 1

hscParseDeclsWithLocation :: HscEnv -> String -> Int -> String -> IO [LHsDecl GhcPs]
hscParseDeclsWithLocation hsc_env source line_num str = do
    L _ (HsModule{ hsmodDecls = decls }) <-
      runInteractiveHsc hsc_env $
        hscParseThingWithLocation source line_num parseModule str
    return decls

-- | Compile a decls
hscDeclsWithLocation :: HscEnv
                     -> String -- ^ The statement
                     -> String -- ^ The source
                     -> Int    -- ^ Starting line
                     -> IO ([TyThing], InteractiveContext)
hscDeclsWithLocation hsc_env str source linenumber = do
    L _ (HsModule{ hsmodDecls = decls }) <-
      runInteractiveHsc hsc_env $
        hscParseThingWithLocation source linenumber parseModule str
    hscParsedDecls hsc_env decls

hscParsedDecls :: HscEnv -> [LHsDecl GhcPs] -> IO ([TyThing], InteractiveContext)
hscParsedDecls hsc_env decls = runInteractiveHsc hsc_env $ do
    hsc_env <- getHscEnv
    let interp = hscInterp hsc_env

    {- Rename and typecheck it -}
    tc_gblenv <- ioMsgMaybe $ tcRnDeclsi hsc_env decls

    {- Grab the new instances -}
    -- We grab the whole environment because of the overlapping that may have
    -- been done. See the notes at the definition of InteractiveContext
    -- (ic_instances) for more details.
    let defaults = tcg_default tc_gblenv

    {- Desugar it -}
    -- We use a basically null location for iNTERACTIVE
    let iNTERACTIVELoc = ModLocation{ ml_hs_file   = Nothing,
                                      ml_hi_file   = panic "hsDeclsWithLocation:ml_hi_file",
                                      ml_obj_file  = panic "hsDeclsWithLocation:ml_obj_file",
                                      ml_hie_file  = panic "hsDeclsWithLocation:ml_hie_file" }
    ds_result <- hscDesugar' iNTERACTIVELoc tc_gblenv

    {- Simplify -}
    simpl_mg <- liftIO $ do
      plugins <- readIORef (tcg_th_coreplugins tc_gblenv)
      hscSimplify hsc_env plugins ds_result

    {- Tidy -}
    (tidy_cg, mod_details) <- liftIO $ tidyProgram hsc_env simpl_mg

    let !CgGuts{ cg_module    = this_mod,
                 cg_binds     = core_binds,
                 cg_tycons    = tycons,
                 cg_modBreaks = mod_breaks } = tidy_cg

        !ModDetails { md_insts     = cls_insts
                    , md_fam_insts = fam_insts } = mod_details
            -- Get the *tidied* cls_insts and fam_insts

        data_tycons = filter isDataTyCon tycons

    {- Prepare For Code Generation -}
    -- Do saturation and convert to A-normal form
    (prepd_binds, _) <- {-# SCC "CorePrep" #-}
      liftIO $ corePrepPgm hsc_env this_mod iNTERACTIVELoc core_binds data_tycons

    (stg_binds, _infotable_prov, _caf_ccs__caf_cc_stacks)
        <- {-# SCC "CoreToStg" #-}
           liftIO $ myCoreToStg (hsc_logger hsc_env)
                                (hsc_dflags hsc_env)
                                (hsc_IC hsc_env)
                                this_mod
                                iNTERACTIVELoc
                                prepd_binds

    {- Generate byte code -}
    cbc <- liftIO $ byteCodeGen hsc_env this_mod
                                stg_binds data_tycons mod_breaks

    let src_span = srcLocSpan interactiveSrcLoc
    _ <- liftIO $ loadDecls interp hsc_env src_span cbc

    {- Load static pointer table entries -}
    liftIO $ hscAddSptEntries hsc_env (cg_spt_entries tidy_cg)

    let tcs = filterOut isImplicitTyCon (mg_tcs simpl_mg)
        patsyns = mg_patsyns simpl_mg

        ext_ids = [ id | id <- bindersOfBinds core_binds
                       , isExternalName (idName id)
                       , not (isDFunId id || isImplicitId id) ]
            -- We only need to keep around the external bindings
            -- (as decided by GHC.Iface.Tidy), since those are the only ones
            -- that might later be looked up by name.  But we can exclude
            --    - DFunIds, which are in 'cls_insts' (see Note [ic_tythings] in GHC.Runtime.Context
            --    - Implicit Ids, which are implicit in tcs
            -- c.f. GHC.Tc.Module.runTcInteractive, which reconstructs the TypeEnv

        new_tythings = map AnId ext_ids ++ map ATyCon tcs ++ map (AConLike . PatSynCon) patsyns
        ictxt        = hsc_IC hsc_env
        -- See Note [Fixity declarations in GHCi]
        fix_env      = tcg_fix_env tc_gblenv
        new_ictxt    = extendInteractiveContext ictxt new_tythings cls_insts
                                                fam_insts defaults fix_env
    return (new_tythings, new_ictxt)

-- | Load the given static-pointer table entries into the interpreter.
-- See Note [Grand plan for static forms] in "GHC.Iface.Tidy.StaticPtrTable".
hscAddSptEntries :: HscEnv -> [SptEntry] -> IO ()
hscAddSptEntries hsc_env entries = do
    let interp = hscInterp hsc_env
    let add_spt_entry :: SptEntry -> IO ()
        add_spt_entry (SptEntry i fpr) = do
            val <- loadName interp hsc_env (idName i)
            addSptEntry interp fpr val
    mapM_ add_spt_entry entries

{-
  Note [Fixity declarations in GHCi]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  To support fixity declarations on types defined within GHCi (as requested
  in #10018) we record the fixity environment in InteractiveContext.
  When we want to evaluate something GHC.Tc.Module.runTcInteractive pulls out this
  fixity environment and uses it to initialize the global typechecker environment.
  After the typechecker has finished its business, an updated fixity environment
  (reflecting whatever fixity declarations were present in the statements we
  passed it) will be returned from hscParsedStmt. This is passed to
  updateFixityEnv, which will stuff it back into InteractiveContext, to be
  used in evaluating the next statement.

-}

hscImport :: HscEnv -> String -> IO (ImportDecl GhcPs)
hscImport hsc_env str = runInteractiveHsc hsc_env $ do
    (L _ (HsModule{hsmodImports=is})) <-
       hscParseThing parseModule str
    case is of
        [L _ i] -> return i
        _ -> liftIO $ throwOneError $
                 mkPlainMsgEnvelope noSrcSpan $
                     text "parse error in import declaration"

-- | Typecheck an expression (but don't run it)
hscTcExpr :: HscEnv
          -> TcRnExprMode
          -> String -- ^ The expression
          -> IO Type
hscTcExpr hsc_env0 mode expr = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  parsed_expr <- hscParseExpr expr
  ioMsgMaybe $ tcRnExpr hsc_env mode parsed_expr

-- | Find the kind of a type, after generalisation
hscKcType
  :: HscEnv
  -> Bool            -- ^ Normalise the type
  -> String          -- ^ The type as a string
  -> IO (Type, Kind) -- ^ Resulting type (possibly normalised) and kind
hscKcType hsc_env0 normalise str = runInteractiveHsc hsc_env0 $ do
    hsc_env <- getHscEnv
    ty <- hscParseType str
    ioMsgMaybe $ tcRnType hsc_env DefaultFlexi normalise ty

hscParseExpr :: String -> Hsc (LHsExpr GhcPs)
hscParseExpr expr = do
  maybe_stmt <- hscParseStmt expr
  case maybe_stmt of
    Just (L _ (BodyStmt _ expr _ _)) -> return expr
    _ -> throwOneError $ mkPlainMsgEnvelope noSrcSpan
      (text "not an expression:" <+> quotes (text expr))

hscParseStmt :: String -> Hsc (Maybe (GhciLStmt GhcPs))
hscParseStmt = hscParseThing parseStmt

hscParseStmtWithLocation :: String -> Int -> String
                         -> Hsc (Maybe (GhciLStmt GhcPs))
hscParseStmtWithLocation source linenumber stmt =
    hscParseThingWithLocation source linenumber parseStmt stmt

hscParseType :: String -> Hsc (LHsType GhcPs)
hscParseType = hscParseThing parseType

hscParseIdentifier :: HscEnv -> String -> IO (LocatedN RdrName)
hscParseIdentifier hsc_env str =
    runInteractiveHsc hsc_env $ hscParseThing parseIdentifier str

hscParseThing :: (Outputable thing, Data thing)
              => Lexer.P thing -> String -> Hsc thing
hscParseThing = hscParseThingWithLocation "<interactive>" 1

hscParseThingWithLocation :: (Outputable thing, Data thing) => String -> Int
                          -> Lexer.P thing -> String -> Hsc thing
hscParseThingWithLocation source linenumber parser str = do
    dflags <- getDynFlags
    logger <- getLogger
    withTiming logger dflags
               (text "Parser [source]")
               (const ()) $ {-# SCC "Parser" #-} do

        let buf = stringToStringBuffer str
            loc = mkRealSrcLoc (fsLit source) linenumber 1

        case unP parser (initParserState (initParserOpts dflags) buf loc) of
            PFailed pst ->
                handleWarningsThrowErrors (getMessages pst)
            POk pst thing -> do
                logWarningsReportErrors (getMessages pst)
                liftIO $ dumpIfSet_dyn logger dflags Opt_D_dump_parsed "Parser"
                            FormatHaskell (ppr thing)
                liftIO $ dumpIfSet_dyn logger dflags Opt_D_dump_parsed_ast "Parser AST"
                            FormatHaskell (showAstData NoBlankSrcSpan NoBlankEpAnnotations thing)
                return thing


{- **********************************************************************
%*                                                                      *
        Desugar, simplify, convert to bytecode, and link an expression
%*                                                                      *
%********************************************************************* -}

hscCompileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO ForeignHValue
hscCompileCoreExpr hsc_env loc expr =
  case hscCompileCoreExprHook (hsc_hooks hsc_env) of
      Nothing -> hscCompileCoreExpr' hsc_env loc expr
      Just h  -> h                   hsc_env loc expr

hscCompileCoreExpr' :: HscEnv -> SrcSpan -> CoreExpr -> IO ForeignHValue
hscCompileCoreExpr' hsc_env srcspan ds_expr
    = do { {- Simplify it -}
           -- Question: should we call SimpleOpt.simpleOptExpr here instead?
           -- It is, well, simpler, and does less inlining etc.
           simpl_expr <- simplifyExpr hsc_env ds_expr

           {- Tidy it (temporary, until coreSat does cloning) -}
         ; let tidy_expr = tidyExpr emptyTidyEnv simpl_expr

           {- Prepare for codegen -}
         ; prepd_expr <- corePrepExpr hsc_env tidy_expr

           {- Lint if necessary -}
         ; lintInteractiveExpr (text "hscCompileExpr") hsc_env prepd_expr
         ; let iNTERACTIVELoc = ModLocation{ ml_hs_file   = Nothing,
                                      ml_hi_file   = panic "hscCompileCoreExpr':ml_hi_file",
                                      ml_obj_file  = panic "hscCompileCoreExpr':ml_obj_file",
                                      ml_hie_file  = panic "hscCompileCoreExpr':ml_hie_file" }

         ; let ictxt = hsc_IC hsc_env
         ; (binding_id, stg_expr, _, _) <-
             myCoreToStgExpr (hsc_logger hsc_env)
                             (hsc_dflags hsc_env)
                             ictxt
                             (icInteractiveModule ictxt)
                             iNTERACTIVELoc
                             prepd_expr

           {- Convert to BCOs -}
         ; bcos <- byteCodeGen hsc_env
                     (icInteractiveModule ictxt)
                     stg_expr
                     [] Nothing

           {- load it -}
         ; fv_hvs <- loadDecls (hscInterp hsc_env) hsc_env srcspan bcos
           {- Get the HValue for the root -}
         ; return (expectJust "hscCompileCoreExpr'"
              $ lookup (idName binding_id) fv_hvs) }


{- **********************************************************************
%*                                                                      *
        Statistics on reading interfaces
%*                                                                      *
%********************************************************************* -}

dumpIfaceStats :: HscEnv -> IO ()
dumpIfaceStats hsc_env = do
    eps <- readIORef (hsc_EPS hsc_env)
    dumpIfSet logger dflags (dump_if_trace || dump_rn_stats)
              "Interface statistics"
              (ifaceStats eps)
  where
    dflags = hsc_dflags hsc_env
    logger = hsc_logger hsc_env
    dump_rn_stats = dopt Opt_D_dump_rn_stats dflags
    dump_if_trace = dopt Opt_D_dump_if_trace dflags


{- **********************************************************************
%*                                                                      *
        Progress Messages: Module i of n
%*                                                                      *
%********************************************************************* -}

showModuleIndex :: (Int, Int) -> SDoc
showModuleIndex (i,n) = text "[" <> pad <> int i <> text " of " <> int n <> text "] "
  where
    -- compute the length of x > 0 in base 10
    len x = ceiling (logBase 10 (fromIntegral x+1) :: Float)
    pad = text (replicate (len n - len i) ' ') -- TODO: use GHC.Utils.Ppr.RStr
