{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-

This module contains miscellaneous functions related to renaming.

-}

module GHC.Rename.Utils (
        checkDupRdrNames, checkDupRdrNamesN, checkShadowedRdrNames,
        checkDupNames, checkDupAndShadowedNames, dupNamesErr,
        checkTupSize, checkCTupSize,
        addFvRn, mapFvRn, mapMaybeFvRn,
        warnUnusedMatches, warnUnusedTypePatterns,
        warnUnusedTopBinds, warnUnusedLocalBinds,
        warnForallIdentifier,
        checkUnusedRecordWildcard,
        mkFieldEnv,
        badQualBndrErr, typeAppErr, badFieldConErr,
        wrapGenSpan, genHsVar, genLHsVar, genHsApp, genHsApps, genAppType,
        genHsIntegralLit, genHsTyLit,

        newLocalBndrRn, newLocalBndrsRn,

        bindLocalNames, bindLocalNamesFV,

        addNameClashErrRn,

        checkInferredVars,
        noNestedForallsContextsErr, addNoNestedForallsContextsErr
)

where


import GHC.Prelude

import GHC.Core.Type
import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr (withHsDocContext)
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Types.Error
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Core.DataCon
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.SourceFile
import GHC.Types.SourceText ( SourceText(..), IntegralLit )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Types.Basic  ( TopLevelFlag(..) )
import GHC.Data.List.SetOps ( removeDups )
import GHC.Data.Maybe ( whenIsJust )
import GHC.Driver.Session
import GHC.Data.FastString
import Control.Monad
import Data.List (find, sortBy)
import GHC.Settings.Constants ( mAX_TUPLE_SIZE, mAX_CTUPLE_SIZE )
import qualified Data.List.NonEmpty as NE
import qualified GHC.LanguageExtensions as LangExt
import GHC.Data.Bag

{-
*********************************************************
*                                                      *
\subsection{Binding}
*                                                      *
*********************************************************
-}

newLocalBndrRn :: LocatedN RdrName -> RnM Name
-- Used for non-top-level binders.  These should
-- never be qualified.
newLocalBndrRn (L loc rdr_name)
  | Just name <- isExact_maybe rdr_name
  = return name -- This happens in code generated by Template Haskell
                -- See Note [Binders in Template Haskell] in "GHC.ThToHs"
  | otherwise
  = do { unless (isUnqual rdr_name)
                (addErrAt (locA loc) (badQualBndrErr rdr_name))
       ; uniq <- newUnique
       ; return (mkInternalName uniq (rdrNameOcc rdr_name) (locA loc)) }

newLocalBndrsRn :: [LocatedN RdrName] -> RnM [Name]
newLocalBndrsRn = mapM newLocalBndrRn

bindLocalNames :: [Name] -> RnM a -> RnM a
bindLocalNames names
  = updLclEnv $ \ lcl_env ->
    let th_level  = thLevel (tcl_th_ctxt lcl_env)
        th_bndrs' = extendNameEnvList (tcl_th_bndrs lcl_env)
                    [ (n, (NotTopLevel, th_level)) | n <- names ]
        rdr_env'  = extendLocalRdrEnvList (tcl_rdr lcl_env) names
    in lcl_env { tcl_th_bndrs = th_bndrs'
               , tcl_rdr      = rdr_env' }

bindLocalNamesFV :: [Name] -> RnM (a, FreeVars) -> RnM (a, FreeVars)
bindLocalNamesFV names enclosed_scope
  = do  { (result, fvs) <- bindLocalNames names enclosed_scope
        ; return (result, delFVs names fvs) }

-------------------------------------
checkDupRdrNames :: [LocatedN RdrName] -> RnM ()
-- Check for duplicated names in a binding group
checkDupRdrNames rdr_names_w_loc
  = mapM_ (dupNamesErr getLocA) dups
  where
    (_, dups) = removeDups (\n1 n2 -> unLoc n1 `compare` unLoc n2) rdr_names_w_loc

checkDupRdrNamesN :: [LocatedN RdrName] -> RnM ()
-- Check for duplicated names in a binding group
checkDupRdrNamesN rdr_names_w_loc
  = mapM_ (dupNamesErr getLocA) dups
  where
    (_, dups) = removeDups (\n1 n2 -> unLoc n1 `compare` unLoc n2) rdr_names_w_loc

checkDupNames :: [Name] -> RnM ()
-- Check for duplicated names in a binding group
checkDupNames names = check_dup_names (filterOut isSystemName names)
                -- See Note [Binders in Template Haskell] in "GHC.ThToHs"

check_dup_names :: [Name] -> RnM ()
check_dup_names names
  = mapM_ (dupNamesErr nameSrcSpan) dups
  where
    (_, dups) = removeDups (\n1 n2 -> nameOccName n1 `compare` nameOccName n2) names

---------------------
checkShadowedRdrNames :: [LocatedN RdrName] -> RnM ()
checkShadowedRdrNames loc_rdr_names
  = do { envs <- getRdrEnvs
       ; checkShadowedOccs envs get_loc_occ filtered_rdrs }
  where
    filtered_rdrs = filterOut (isExact . unLoc) loc_rdr_names
                -- See Note [Binders in Template Haskell] in "GHC.ThToHs"
    get_loc_occ (L loc rdr) = (locA loc,rdrNameOcc rdr)

checkDupAndShadowedNames :: (GlobalRdrEnv, LocalRdrEnv) -> [Name] -> RnM ()
checkDupAndShadowedNames envs names
  = do { check_dup_names filtered_names
       ; checkShadowedOccs envs get_loc_occ filtered_names }
  where
    filtered_names = filterOut isSystemName names
                -- See Note [Binders in Template Haskell] in "GHC.ThToHs"
    get_loc_occ name = (nameSrcSpan name, nameOccName name)

-------------------------------------
checkShadowedOccs :: (GlobalRdrEnv, LocalRdrEnv)
                  -> (a -> (SrcSpan, OccName))
                  -> [a] -> RnM ()
checkShadowedOccs (global_env,local_env) get_loc_occ ns
  = whenWOptM Opt_WarnNameShadowing $
    do  { traceRn "checkShadowedOccs:shadow" (ppr (map get_loc_occ ns))
        ; mapM_ check_shadow ns }
  where
    check_shadow n
        | startsWithUnderscore occ = return ()  -- Do not report shadowing for "_x"
                                                -- See #3262
        | Just n <- mb_local = complain (ShadowedNameProvenanceLocal (nameSrcLoc n))
        | otherwise = do { gres' <- filterM is_shadowed_gre gres
                         ; when (not . null $ gres') $ complain (ShadowedNameProvenanceGlobal gres') }
        where
          (loc,occ) = get_loc_occ n
          mb_local  = lookupLocalRdrOcc local_env occ
          gres      = lookupGRE_RdrName (mkRdrUnqual occ) global_env
                -- Make an Unqualified RdrName and look that up, so that
                -- we don't find any GREs that are in scope qualified-only

          complain provenance = addDiagnosticAt loc (TcRnShadowedName occ provenance)

    is_shadowed_gre :: GlobalRdrElt -> RnM Bool
        -- Returns False for record selectors that are shadowed, when
        -- punning or wild-cards are on (cf #2723)
    is_shadowed_gre gre | isRecFldGRE gre
        = do { dflags <- getDynFlags
             ; return $ not (xopt LangExt.NamedFieldPuns dflags
                             || xopt LangExt.RecordWildCards dflags) }
    is_shadowed_gre _other = return True

-------------------------------------
-- | Throw an error message if a user attempts to quantify an inferred type
-- variable in a place where specificity cannot be observed. For example,
-- @forall {a}. [a] -> [a]@ would be rejected to the inferred type variable
-- @{a}@, but @forall a. [a] -> [a]@ would be accepted.
-- See @Note [Unobservably inferred type variables]@.
checkInferredVars :: HsDocContext
                  -> Maybe SDoc
                  -- ^ The error msg if the signature is not allowed to contain
                  --   manually written inferred variables.
                  -> LHsSigType GhcPs
                  -> RnM ()
checkInferredVars _    Nothing    _  = return ()
checkInferredVars ctxt (Just msg) ty =
  let bndrs = sig_ty_bndrs ty
  in case find ((==) InferredSpec . hsTyVarBndrFlag) bndrs of
    Nothing -> return ()
    Just _  -> addErr $ TcRnUnknownMessage $ mkPlainError noHints (withHsDocContext ctxt msg)
  where
    sig_ty_bndrs :: LHsSigType GhcPs -> [HsTyVarBndr Specificity GhcPs]
    sig_ty_bndrs (L _ (HsSig{sig_bndrs = outer_bndrs}))
      = map unLoc (hsOuterExplicitBndrs outer_bndrs)

{-
Note [Unobservably inferred type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While GHC's parser allows the use of inferred type variables
(e.g., `forall {a}. <...>`) just about anywhere that type variable binders can
appear, there are some situations where the distinction between inferred and
specified type variables cannot be observed. For example, consider this
instance declaration:

  instance forall {a}. Eq (T a) where ...

Making {a} inferred is pointless, as there is no way for user code to
"apply" an instance declaration in a way where the inferred/specified
distinction would make a difference. (Notably, there is no opportunity
for visible type application of an instance declaration.) Anyone who
writes such code is likely confused, so in an attempt to be helpful,
we emit an error message if a user writes code like this. The
checkInferredVars function is responsible for implementing this
restriction.

It turns out to be somewhat cumbersome to enforce this restriction in
certain cases.  Specifically:

* Quantified constraints. In the type `f :: (forall {a}. C a) => Proxy Int`,
  there is no way to observe that {a} is inferred. Nevertheless, actually
  rejecting this code would be tricky, as we would need to reject
  `forall {a}. <...>` as a constraint but *accept* other uses of
  `forall {a}. <...>` as a type (e.g., `g :: (forall {a}. a -> a) -> b -> b`).
  This is quite tedious to do in practice, so we don't bother.

* Default method type signatures (#18432). These are tricky because inferred
  type variables can appear nested, e.g.,

    class C a where
      m         :: forall b. a -> b -> forall c.   c -> c
      default m :: forall b. a -> b -> forall {c}. c -> c
      m _ _ = id

  Robustly checking for nested, inferred type variables ends up being a pain,
  so we don't try to do this.

For now, we simply allow inferred quantifiers to be specified here,
even though doing so is pointless. All we lose is a warning.

Aside from the places where we already use checkInferredVars, most of
the other places where inferred vars don't make sense are in any case
already prohibited from having foralls /at all/.  For example:

  instance forall a. forall {b}. Eq (Either a b) where ...

Here the nested `forall {b}` is already prohibited. (See
Note [No nested foralls or contexts in instance types] in GHC.Hs.Type).
-}

-- | Examines a non-outermost type for @forall@s or contexts, which are assumed
-- to be nested. For example, in the following declaration:
--
-- @
-- instance forall a. forall b. C (Either a b)
-- @
--
-- The outermost @forall a@ is fine, but the nested @forall b@ is not. We
-- invoke 'noNestedForallsContextsErr' on the type @forall b. C (Either a b)@
-- to catch the nested @forall@ and create a suitable error message.
-- 'noNestedForallsContextsErr' returns @'Just' err_msg@ if such a @forall@ or
-- context is found, and returns @Nothing@ otherwise.
--
-- This is currently used in the following places:
--
-- * In GADT constructor types (in 'rnConDecl').
--   See @Note [GADT abstract syntax] (Wrinkle: No nested foralls or contexts)@
--   in "Language.Haskell.Syntax.Decls".
--
-- * In instance declaration types (in 'rnClsIntDecl' and 'rnSrcDerivDecl' in
--   "GHC.Rename.Module" and 'renameSig' in "GHC.Rename.Bind").
--   See @Note [No nested foralls or contexts in instance types]@ in
--   "GHC.Hs.Type".
noNestedForallsContextsErr :: SDoc -> LHsType GhcRn -> Maybe (SrcSpan, SDoc)
noNestedForallsContextsErr what lty =
  case ignoreParens lty of
    L l (HsForAllTy { hst_tele = tele })
      |  HsForAllVis{} <- tele
         -- The only two places where this function is called correspond to
         -- types of terms, so we give a slightly more descriptive error
         -- message in the event that they contain visible dependent
         -- quantification (currently only allowed in kinds).
      -> Just (locA l, vcat [ text "Illegal visible, dependent quantification" <+>
                              text "in the type of a term"
                            , text "(GHC does not yet support this)" ])
      |  HsForAllInvis{} <- tele
      -> Just (locA l, nested_foralls_contexts_err)
    L l (HsQualTy {})
      -> Just (locA l, nested_foralls_contexts_err)
    _ -> Nothing
  where
    nested_foralls_contexts_err =
      what <+> text "cannot contain nested"
      <+> quotes forAllLit <> text "s or contexts"

-- | A common way to invoke 'noNestedForallsContextsErr'.
addNoNestedForallsContextsErr :: HsDocContext -> SDoc -> LHsType GhcRn -> RnM ()
addNoNestedForallsContextsErr ctxt what lty =
  whenIsJust (noNestedForallsContextsErr what lty) $ \(l, err_msg) ->
    addErrAt l $ TcRnUnknownMessage $ mkPlainError noHints (withHsDocContext ctxt err_msg)

{-
************************************************************************
*                                                                      *
\subsection{Free variable manipulation}
*                                                                      *
************************************************************************
-}

-- A useful utility
addFvRn :: FreeVars -> RnM (thing, FreeVars) -> RnM (thing, FreeVars)
addFvRn fvs1 thing_inside = do { (res, fvs2) <- thing_inside
                               ; return (res, fvs1 `plusFV` fvs2) }

mapFvRn :: (a -> RnM (b, FreeVars)) -> [a] -> RnM ([b], FreeVars)
mapFvRn f xs = do stuff <- mapM f xs
                  case unzip stuff of
                      (ys, fvs_s) -> return (ys, plusFVs fvs_s)

mapMaybeFvRn :: (a -> RnM (b, FreeVars)) -> Maybe a -> RnM (Maybe b, FreeVars)
mapMaybeFvRn _ Nothing = return (Nothing, emptyFVs)
mapMaybeFvRn f (Just x) = do { (y, fvs) <- f x; return (Just y, fvs) }

{-
************************************************************************
*                                                                      *
\subsection{Envt utility functions}
*                                                                      *
************************************************************************
-}

warnUnusedTopBinds :: [GlobalRdrElt] -> RnM ()
warnUnusedTopBinds gres
    = whenWOptM Opt_WarnUnusedTopBinds
    $ do env <- getGblEnv
         let isBoot = tcg_src env == HsBootFile
         let noParent gre = case gre_par gre of
                            NoParent -> True
                            _        -> False
             -- Don't warn about unused bindings with parents in
             -- .hs-boot files, as you are sometimes required to give
             -- unused bindings (trac #3449).
             -- HOWEVER, in a signature file, you are never obligated to put a
             -- definition in the main text.  Thus, if you define something
             -- and forget to export it, we really DO want to warn.
             gres' = if isBoot then filter noParent gres
                               else                 gres
         warnUnusedGREs gres'


-- | Checks to see if we need to warn for -Wunused-record-wildcards or
-- -Wredundant-record-wildcards
checkUnusedRecordWildcard :: SrcSpan
                          -> FreeVars
                          -> Maybe [Name]
                          -> RnM ()
checkUnusedRecordWildcard _ _ Nothing     = return ()
checkUnusedRecordWildcard loc _ (Just []) =
  -- Add a new warning if the .. pattern binds no variables
  setSrcSpan loc $ warnRedundantRecordWildcard
checkUnusedRecordWildcard loc fvs (Just dotdot_names) =
  setSrcSpan loc $ warnUnusedRecordWildcard dotdot_names fvs


-- | Produce a warning when the `..` pattern binds no new
-- variables.
--
-- @
--   data P = P { x :: Int }
--
--   foo (P{x, ..}) = x
-- @
--
-- The `..` here doesn't bind any variables as `x` is already bound.
warnRedundantRecordWildcard :: RnM ()
warnRedundantRecordWildcard =
  whenWOptM Opt_WarnRedundantRecordWildcards $
    let msg = TcRnUnknownMessage $
                mkPlainDiagnostic (WarningWithFlag Opt_WarnRedundantRecordWildcards)
                                  noHints
                                  redundantWildcardWarning
    in addDiagnostic msg


-- | Produce a warning when no variables bound by a `..` pattern are used.
--
-- @
--   data P = P { x :: Int }
--
--   foo (P{..}) = ()
-- @
--
-- The `..` pattern binds `x` but it is not used in the RHS so we issue
-- a warning.
warnUnusedRecordWildcard :: [Name] -> FreeVars -> RnM ()
warnUnusedRecordWildcard ns used_names = do
  let used = filter (`elemNameSet` used_names) ns
  traceRn "warnUnused" (ppr ns $$ ppr used_names $$ ppr used)
  warnIf (null used)
    unusedRecordWildcardWarning



warnUnusedLocalBinds, warnUnusedMatches, warnUnusedTypePatterns
  :: [Name] -> FreeVars -> RnM ()
warnUnusedLocalBinds   = check_unused Opt_WarnUnusedLocalBinds
warnUnusedMatches      = check_unused Opt_WarnUnusedMatches
warnUnusedTypePatterns = check_unused Opt_WarnUnusedTypePatterns

check_unused :: WarningFlag -> [Name] -> FreeVars -> RnM ()
check_unused flag bound_names used_names
  = whenWOptM flag (warnUnused flag (filterOut (`elemNameSet` used_names)
                                               bound_names))

warnForallIdentifier :: LocatedN RdrName -> RnM ()
warnForallIdentifier (L l rdr_name@(Unqual occ))
  | isKw (fsLit "forall") || isKw (fsLit "∀")
  = addDiagnosticAt (locA l) (TcRnForallIdentifier rdr_name)
  where isKw = (occNameFS occ ==)
warnForallIdentifier _ = return ()

-------------------------
--      Helpers
warnUnusedGREs :: [GlobalRdrElt] -> RnM ()
warnUnusedGREs gres = mapM_ warnUnusedGRE gres

-- NB the Names must not be the names of record fields!
warnUnused :: WarningFlag -> [Name] -> RnM ()
warnUnused flag names =
    mapM_ (warnUnused1 flag . NormalGreName) names

warnUnused1 :: WarningFlag -> GreName -> RnM ()
warnUnused1 flag child
  = when (reportable child) $
    addUnusedWarning flag
                     (occName child) (greNameSrcSpan child)
                     (text $ "Defined but not used" ++ opt_str)
  where
    opt_str = case flag of
                Opt_WarnUnusedTypePatterns -> " on the right hand side"
                _ -> ""

warnUnusedGRE :: GlobalRdrElt -> RnM ()
warnUnusedGRE gre@(GRE { gre_lcl = lcl, gre_imp = is })
  | lcl       = warnUnused1 Opt_WarnUnusedTopBinds (gre_name gre)
  | otherwise = when (reportable (gre_name gre)) (mapM_ warn is)
  where
    occ = greOccName gre
    warn spec = addUnusedWarning Opt_WarnUnusedTopBinds occ span msg
        where
           span = importSpecLoc spec
           pp_mod = quotes (ppr (importSpecModule spec))
           msg = text "Imported from" <+> pp_mod <+> text "but not used"

-- | Make a map from selector names to field labels and parent tycon
-- names, to be used when reporting unused record fields.
mkFieldEnv :: GlobalRdrEnv -> NameEnv (FieldLabelString, Parent)
mkFieldEnv rdr_env = mkNameEnv [ (greMangledName gre, (flLabel fl, gre_par gre))
                               | gres <- nonDetOccEnvElts rdr_env
                               , gre <- gres
                               , Just fl <- [greFieldLabel gre]
                               ]

-- | Should we report the fact that this 'Name' is unused? The
-- 'OccName' may differ from 'nameOccName' due to
-- DuplicateRecordFields.
reportable :: GreName -> Bool
reportable child
  | NormalGreName name <- child
  , isWiredInName name = False    -- Don't report unused wired-in names
                                  -- Otherwise we get a zillion warnings
                                  -- from Data.Tuple
  | otherwise = not (startsWithUnderscore (occName child))

addUnusedWarning :: WarningFlag -> OccName -> SrcSpan -> SDoc -> RnM ()
addUnusedWarning flag occ span msg = do
  let diag = TcRnUnknownMessage $ mkPlainDiagnostic (WarningWithFlag flag) noHints $
        sep [msg <> colon,
             nest 2 $ pprNonVarNameSpace (occNameSpace occ)
                            <+> quotes (ppr occ)]
  addDiagnosticAt span diag

unusedRecordWildcardWarning :: TcRnMessage
unusedRecordWildcardWarning =
  TcRnUnknownMessage $ mkPlainDiagnostic (WarningWithFlag Opt_WarnUnusedRecordWildcards) noHints $
    wildcardDoc $ text "No variables bound in the record wildcard match are used"

redundantWildcardWarning :: SDoc
redundantWildcardWarning =
  wildcardDoc $ text "Record wildcard does not bind any new variables"

wildcardDoc :: SDoc -> SDoc
wildcardDoc herald =
  herald
    $$ nest 2 (text "Possible fix" <> colon <+> text "omit the"
                                            <+> quotes (text ".."))

{-
Note [Skipping ambiguity errors at use sites of local declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, we do not report ambiguous occurrences at use sites where all the
clashing names are defined locally, because the error will have been reported at
the definition site, and we want to avoid an error cascade.

However, when DuplicateRecordFields is enabled, it is possible to define the
same field name multiple times, so we *do* need to report an error at the use
site when there is ambiguity between multiple fields. Moreover, when
NoFieldSelectors is enabled, it is possible to define a field with the same name
as a non-field, so again we need to report ambiguity at the use site.

We can skip reporting an ambiguity error whenever defining the GREs must have
yielded a duplicate declarations error.  More precisely, we can skip if:

 * there are at least two non-fields amongst the GREs; or

 * there are at least two fields amongst the GREs, and DuplicateRecordFields is
   *disabled*; or

 * there is at least one non-field, at least one field, and NoFieldSelectors is
   *disabled*.

These conditions ensure that a duplicate local declaration will have been
reported.  See also Note [Reporting duplicate local declarations] in
GHC.Rename.Names).

-}

addNameClashErrRn :: RdrName -> NE.NonEmpty GlobalRdrElt -> RnM ()
addNameClashErrRn rdr_name gres
  | all isLocalGRE gres && can_skip
  -- If there are two or more *local* defns, we'll usually have reported that
  -- already, and we don't want an error cascade.
  = return ()
  | otherwise
  = addErr $ TcRnUnknownMessage $ mkPlainError noHints $
    (vcat [ text "Ambiguous occurrence" <+> quotes (ppr rdr_name)
                 , text "It could refer to"
                 , nest 3 (vcat (msg1 : msgs)) ])
  where
    np1 NE.:| nps = gres
    msg1 =  text "either" <+> ppr_gre np1
    msgs = [text "    or" <+> ppr_gre np | np <- nps]
    ppr_gre gre = sep [ pp_greMangledName gre <> comma
                      , pprNameProvenance gre]

    -- When printing the name, take care to qualify it in the same
    -- way as the provenance reported by pprNameProvenance, namely
    -- the head of 'gre_imp'.  Otherwise we get confusing reports like
    --   Ambiguous occurrence ‘null’
    --   It could refer to either ‘T15487a.null’,
    --                            imported from ‘Prelude’ at T15487.hs:1:8-13
    --                     or ...
    -- See #15487
    pp_greMangledName gre@(GRE { gre_name = child
                         , gre_lcl = lcl, gre_imp = iss }) =
      case child of
        FieldGreName fl  -> text "the field" <+> quotes (ppr fl)
        NormalGreName name -> quotes (pp_qual name <> dot <> ppr (nameOccName name))
      where
        pp_qual name
                | lcl
                = ppr (nameModule name)
                | Just imp  <- headMaybe iss  -- This 'imp' is the one that
                                  -- pprNameProvenance chooses
                , ImpDeclSpec { is_as = mod } <- is_decl imp
                = ppr mod
                | otherwise
                = pprPanic "addNameClassErrRn" (ppr gre $$ ppr iss)
                  -- Invariant: either 'lcl' is True or 'iss' is non-empty

    -- If all the GREs are defined locally, can we skip reporting an ambiguity
    -- error at use sites, because it will have been reported already? See
    -- Note [Skipping ambiguity errors at use sites of local declarations]
    can_skip = num_non_flds >= 2
            || (num_flds >= 2 && not (isDuplicateRecFldGRE (head flds)))
            || (num_non_flds >= 1 && num_flds >= 1
                                  && not (isNoFieldSelectorGRE (head flds)))
    (flds, non_flds) = NE.partition isRecFldGRE gres
    num_flds     = length flds
    num_non_flds = length non_flds


dupNamesErr :: Outputable n => (n -> SrcSpan) -> NE.NonEmpty n -> RnM ()
dupNamesErr get_loc names
  = addErrAt big_loc $ TcRnUnknownMessage $ mkPlainError noHints $
    vcat [text "Conflicting definitions for" <+> quotes (ppr (NE.head names)),
          locations]
  where
    locs      = map get_loc (NE.toList names)
    big_loc   = foldr1 combineSrcSpans locs
    locations = text "Bound at:" <+> vcat (map ppr (sortBy SrcLoc.leftmost_smallest locs))

badQualBndrErr :: RdrName -> TcRnMessage
badQualBndrErr rdr_name
  = TcRnUnknownMessage $ mkPlainError noHints $
  text "Qualified name in binding position:" <+> ppr rdr_name

typeAppErr :: String -> LHsType GhcPs -> TcRnMessage
typeAppErr what (L _ k)
  = TcRnUnknownMessage $ mkPlainError noHints $
    hang (text "Illegal visible" <+> text what <+> text "application"
            <+> quotes (char '@' <> ppr k))
       2 (text "Perhaps you intended to use TypeApplications")

badFieldConErr :: Name -> FieldLabelString -> TcRnMessage
badFieldConErr con field
  = TcRnUnknownMessage $ mkPlainError noHints $
    hsep [text "Constructor" <+> quotes (ppr con),
          text "does not have field", quotes (ppr field)]

-- | Ensure that a boxed or unboxed tuple has arity no larger than
-- 'mAX_TUPLE_SIZE'.
checkTupSize :: Int -> TcM ()
checkTupSize tup_size
  | tup_size <= mAX_TUPLE_SIZE
  = return ()
  | otherwise
  = addErr $ TcRnUnknownMessage $ mkPlainError noHints $
    sep [text "A" <+> int tup_size <> text "-tuple is too large for GHC",
                 nest 2 (parens (text "max size is" <+> int mAX_TUPLE_SIZE)),
                 nest 2 (text "Workaround: use nested tuples or define a data type")]

-- | Ensure that a constraint tuple has arity no larger than 'mAX_CTUPLE_SIZE'.
checkCTupSize :: Int -> TcM ()
checkCTupSize tup_size
  | tup_size <= mAX_CTUPLE_SIZE
  = return ()
  | otherwise
  = addErr $ TcRnUnknownMessage $ mkPlainError noHints $
    hang (text "Constraint tuple arity too large:" <+> int tup_size
                  <+> parens (text "max arity =" <+> int mAX_CTUPLE_SIZE))
               2 (text "Instead, use a nested tuple")

{- *********************************************************************
*                                                                      *
              Generating code for HsExpanded
      See Note [Handling overloaded and rebindable constructs]
*                                                                      *
********************************************************************* -}

wrapGenSpan :: a -> LocatedAn an a
-- Wrap something in a "generatedSrcSpan"
-- See Note [Rebindable syntax and HsExpansion]
wrapGenSpan x = L (noAnnSrcSpan generatedSrcSpan) x

genHsApps :: Name -> [LHsExpr GhcRn] -> HsExpr GhcRn
genHsApps fun args = foldl genHsApp (genHsVar fun) args

genHsApp :: HsExpr GhcRn -> LHsExpr GhcRn -> HsExpr GhcRn
genHsApp fun arg = HsApp noAnn (wrapGenSpan fun) arg

genLHsVar :: Name -> LHsExpr GhcRn
genLHsVar nm = wrapGenSpan $ genHsVar nm

genHsVar :: Name -> HsExpr GhcRn
genHsVar nm = HsVar noExtField $ wrapGenSpan nm

genAppType :: HsExpr GhcRn -> HsType (NoGhcTc GhcRn) -> HsExpr GhcRn
genAppType expr = HsAppType noExtField (wrapGenSpan expr) . mkEmptyWildCardBndrs . wrapGenSpan

genHsIntegralLit :: IntegralLit -> LocatedAn an (HsExpr GhcRn)
genHsIntegralLit lit = wrapGenSpan $ HsLit noAnn (HsInt noExtField lit)

genHsTyLit :: FastString -> HsType GhcRn
genHsTyLit = HsTyLit noExtField . HsStrTy NoSourceText
