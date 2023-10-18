{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Tc.Gen.Head
       ( HsExprArg(..), EValArg(..), TcPass(..)
       , AppCtxt(..), appCtxtLoc, insideExpansion
       , splitHsApps, rebuildHsApps
       , addArgWrap, isHsValArg
       , countLeadingValArgs, isVisibleArg, pprHsExprArgTc

       , tcInferAppHead, tcInferAppHead_maybe
       , tcInferId, tcCheckId
       , obviousSig, addAmbiguousNameErr
       , tyConOf, tyConOfET, lookupParents, fieldNotInType
       , notSelector, nonBidirectionalErr

       , addExprCtxt, addFunResCtxt ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcExpr, tcCheckMonoExprNC, tcCheckPolyExprNC )

import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Pat
import GHC.Tc.Gen.Bind( chooseInferredQuantifiers )
import GHC.Tc.Gen.Sig( tcUserTypeSig, tcInstSig )
import GHC.Tc.TyCl.PatSyn( patSynBuilderOcc )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Types.Basic
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs, tcLookupDataFamInst )
import GHC.Core.FamInstEnv    ( FamInstEnvs )
import GHC.Core.UsageEnv      ( unitUE )
import GHC.Rename.Env         ( addUsedGRE )
import GHC.Rename.Utils       ( addNameClashErrRn, unknownSubordinateErr )
import GHC.Tc.Solver          ( InferMode(..), simplifyInfer )
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Zonk      ( hsLitType )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Hs
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Tc.Types.Evidence
import GHC.Builtin.Types( multiplicityTy )
import GHC.Builtin.Names
import GHC.Builtin.Names.TH( liftStringName, liftName )
import GHC.Driver.Session
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import Control.Monad

import Data.Function
import qualified Data.List.NonEmpty as NE

#include "HsVersions.h"

import GHC.Prelude


{- *********************************************************************
*                                                                      *
              HsExprArg: auxiliary data type
*                                                                      *
********************************************************************* -}

{- Note [HsExprArg]
~~~~~~~~~~~~~~~~~~~
The data type HsExprArg :: TcPass -> Type
is a very local type, used only within this module and GHC.Tc.Gen.App

* It's really a zipper for an application chain
  See Note [Application chains and heads] in GHC.Tc.Gen.App for
  what an "application chain" is.

* It's a GHC-specific type, so using TTG only where necessary

* It is indexed by TcPass, meaning
  - HsExprArg TcpRn:
      The result of splitHsApps, which decomposes a HsExpr GhcRn

  - HsExprArg TcpInst:
      The result of tcInstFun, which instantiates the function type
      Adds EWrap nodes, the argument type in EValArg,
      and the kind-checked type in ETypeArg

  - HsExprArg TcpTc:
      The result of tcArg, which typechecks the value args
      In EValArg we now have a (LHsExpr GhcTc)

* rebuildPrefixApps is dual to splitHsApps, and zips an application
  back into a HsExpr

Note [EValArg]
~~~~~~~~~~~~~~
The data type EValArg is the payload of the EValArg constructor of
HsExprArg; i.e. a value argument of the application.  EValArg has two
forms:

* ValArg: payload is just the expression itself. Simple.

* ValArgQL: captures the results of applying quickLookArg to the
  argument in a ValArg.  When we later want to typecheck that argument
  we can just carry on from where quick-look left off.  The fields of
  ValArgQL exactly capture what is needed to complete the job.

Invariants:

1. With QL switched off, all arguments are ValArg; no ValArgQL

2. With QL switched on, tcInstFun converts some ValArgs to ValArgQL,
   under the conditions when quick-look should happen (eg the argument
   type is guarded) -- see quickLookArg

Note [splitHsApps]
~~~~~~~~~~~~~~~~~~
The key function
  splitHsApps :: HsExpr GhcRn -> (HsExpr GhcRn, HsExpr GhcRn, [HsExprArg 'TcpRn])
takes apart either an HsApp, or an infix OpApp, returning

* The "head" of the application, an expression that is often a variable;
  this is used for typechecking

* The "user head" or "error head" of the application, to be reported to the
  user in case of an error.  Example:
         (`op` e)
  expands (via HsExpanded) to
         (rightSection op e)
  but we don't want to see 'rightSection' in error messages. So we keep the
  innermost un-expanded head as the "error head".

* A list of HsExprArg, the arguments
-}

data TcPass = TcpRn     -- Arguments decomposed
            | TcpInst   -- Function instantiated
            | TcpTc     -- Typechecked

data HsExprArg (p :: TcPass)
  = -- See Note [HsExprArg]
    EValArg  { eva_ctxt   :: AppCtxt
             , eva_arg    :: EValArg p
             , eva_arg_ty :: !(XEVAType p) }

  | ETypeArg { eva_ctxt  :: AppCtxt
             , eva_hs_ty :: LHsWcType GhcRn  -- The type arg
             , eva_ty    :: !(XETAType p) }  -- Kind-checked type arg

  | EPrag    AppCtxt
             (HsPragE (GhcPass (XPass p)))

  | EWrap    EWrap

data EWrap = EPar    AppCtxt
           | EExpand (HsExpr GhcRn)
           | EHsWrap HsWrapper

data EValArg (p :: TcPass) where  -- See Note [EValArg]
  ValArg   :: LHsExpr (GhcPass (XPass p))
           -> EValArg p

  ValArgQL :: { va_expr :: LHsExpr GhcRn        -- Original application
                                                -- For location and error msgs
              , va_fun  :: (HsExpr GhcTc, AppCtxt) -- Function of the application,
                                                   -- typechecked, plus its context
              , va_args :: [HsExprArg 'TcpInst] -- Args, instantiated
              , va_ty   :: TcRhoType }          -- Result type
           -> EValArg 'TcpInst  -- Only exists in TcpInst phase

data AppCtxt
  = VAExpansion
       (HsExpr GhcRn)    -- Inside an expansion of this expression
       SrcSpan           -- The SrcSpan of the expression
                         --    noSrcSpan if outermost

  | VACall
       (HsExpr GhcRn) Int  -- In the third argument of function f
       SrcSpan             -- The SrcSpan of the application (f e1 e2 e3)

appCtxtLoc :: AppCtxt -> SrcSpan
appCtxtLoc (VAExpansion _ l) = l
appCtxtLoc (VACall _ _ l)    = l

insideExpansion :: AppCtxt -> Bool
insideExpansion (VAExpansion {}) = True
insideExpansion (VACall {})      = False

instance Outputable AppCtxt where
  ppr (VAExpansion e _) = text "VAExpansion" <+> ppr e
  ppr (VACall f n _)    = text "VACall" <+> int n <+> ppr f

type family XPass p where
  XPass 'TcpRn   = 'Renamed
  XPass 'TcpInst = 'Renamed
  XPass 'TcpTc   = 'Typechecked

type family XETAType p where  -- Type arguments
  XETAType 'TcpRn = NoExtField
  XETAType _      = Type

type family XEVAType p where  -- Value arguments
  XEVAType 'TcpRn = NoExtField
  XEVAType _      = Scaled Type

mkEValArg :: AppCtxt -> LHsExpr GhcRn -> HsExprArg 'TcpRn
mkEValArg ctxt e = EValArg { eva_arg = ValArg e, eva_ctxt = ctxt
                           , eva_arg_ty = noExtField }

mkETypeArg :: AppCtxt -> LHsWcType GhcRn -> HsExprArg 'TcpRn
mkETypeArg ctxt hs_ty = ETypeArg { eva_ctxt = ctxt, eva_hs_ty = hs_ty
                                 , eva_ty = noExtField }

addArgWrap :: HsWrapper -> [HsExprArg 'TcpInst] -> [HsExprArg 'TcpInst]
addArgWrap wrap args
 | isIdHsWrapper wrap = args
 | otherwise          = EWrap (EHsWrap wrap) : args

splitHsApps :: HsExpr GhcRn
            -> ( (HsExpr GhcRn, AppCtxt)  -- Head
               , [HsExprArg 'TcpRn])      -- Args
-- See Note [splitHsApps]
splitHsApps e = go e (top_ctxt 0 e) []
  where
    top_ctxt n (HsPar _ fun)               = top_lctxt n fun
    top_ctxt n (HsPragE _ _ fun)           = top_lctxt n fun
    top_ctxt n (HsAppType _ fun _)         = top_lctxt (n+1) fun
    top_ctxt n (HsApp _ fun _)             = top_lctxt (n+1) fun
    top_ctxt n (XExpr (HsExpanded orig _)) = VACall orig      n noSrcSpan
    top_ctxt n other_fun                   = VACall other_fun n noSrcSpan

    top_lctxt n (L _ fun) = top_ctxt n fun

    go :: HsExpr GhcRn -> AppCtxt -> [HsExprArg 'TcpRn]
       -> ((HsExpr GhcRn, AppCtxt), [HsExprArg 'TcpRn])
    go (HsPar _     (L l fun))    ctxt args = go fun (set l ctxt) (EWrap (EPar ctxt)   : args)
    go (HsPragE _ p (L l fun))    ctxt args = go fun (set l ctxt) (EPrag      ctxt p   : args)
    go (HsAppType _ (L l fun) ty) ctxt args = go fun (dec l ctxt) (mkETypeArg ctxt ty  : args)
    go (HsApp _ (L l fun) arg)    ctxt args = go fun (dec l ctxt) (mkEValArg  ctxt arg : args)

    -- See Note [Looking through HsExpanded]
    go (XExpr (HsExpanded orig fun)) ctxt args
      = go fun (VAExpansion orig (appCtxtLoc ctxt)) (EWrap (EExpand orig) : args)

    -- See Note [Desugar OpApp in the typechecker]
    go e@(OpApp _ arg1 (L l op) arg2) _ args
      = ( (op, VACall op 0 (locA l))
        ,   mkEValArg (VACall op 1 generatedSrcSpan) arg1
          : mkEValArg (VACall op 2 generatedSrcSpan) arg2
          : EWrap (EExpand e)
          : args )

    go e ctxt args = ((e,ctxt), args)

    set :: SrcSpanAnnA -> AppCtxt -> AppCtxt
    set l (VACall f n _)        = VACall f n (locA l)
    set _ ctxt@(VAExpansion {}) = ctxt

    dec :: SrcSpanAnnA -> AppCtxt -> AppCtxt
    dec l (VACall f n _)        = VACall f (n-1) (locA l)
    dec _ ctxt@(VAExpansion {}) = ctxt

rebuildHsApps :: HsExpr GhcTc -> AppCtxt -> [HsExprArg 'TcpTc]-> HsExpr GhcTc
rebuildHsApps fun _ [] = fun
rebuildHsApps fun ctxt (arg : args)
  = case arg of
      EValArg { eva_arg = ValArg arg, eva_ctxt = ctxt' }
        -> rebuildHsApps (HsApp noAnn lfun arg) ctxt' args
      ETypeArg { eva_hs_ty = hs_ty, eva_ty  = ty, eva_ctxt = ctxt' }
        -> rebuildHsApps (HsAppType ty lfun hs_ty) ctxt' args
      EPrag ctxt' p
        -> rebuildHsApps (HsPragE noExtField p lfun) ctxt' args
      EWrap (EPar ctxt')
        -> rebuildHsApps (HsPar noAnn lfun) ctxt' args
      EWrap (EExpand orig)
        -> rebuildHsApps (XExpr (ExpansionExpr (HsExpanded orig fun))) ctxt args
      EWrap (EHsWrap wrap)
        -> rebuildHsApps (mkHsWrap wrap fun) ctxt args
  where
    lfun = L (noAnnSrcSpan $ appCtxtLoc ctxt) fun

isHsValArg :: HsExprArg id -> Bool
isHsValArg (EValArg {}) = True
isHsValArg _            = False

countLeadingValArgs :: [HsExprArg id] -> Int
countLeadingValArgs []                   = 0
countLeadingValArgs (EValArg {}  : args) = 1 + countLeadingValArgs args
countLeadingValArgs (EWrap {}    : args) = countLeadingValArgs args
countLeadingValArgs (EPrag {}    : args) = countLeadingValArgs args
countLeadingValArgs (ETypeArg {} : _)    = 0

isValArg :: HsExprArg id -> Bool
isValArg (EValArg {}) = True
isValArg _            = False

isVisibleArg :: HsExprArg id -> Bool
isVisibleArg (EValArg {})  = True
isVisibleArg (ETypeArg {}) = True
isVisibleArg _             = False

instance OutputableBndrId (XPass p) => Outputable (HsExprArg p) where
  ppr (EValArg { eva_arg = arg })      = text "EValArg" <+> ppr arg
  ppr (EPrag _ p)                      = text "EPrag" <+> ppr p
  ppr (ETypeArg { eva_hs_ty = hs_ty }) = char '@' <> ppr hs_ty
  ppr (EWrap wrap)                     = ppr wrap

instance Outputable EWrap where
  ppr (EPar _)       = text "EPar"
  ppr (EHsWrap w)    = text "EHsWrap" <+> ppr w
  ppr (EExpand orig) = text "EExpand" <+> ppr orig

instance OutputableBndrId (XPass p) => Outputable (EValArg p) where
  ppr (ValArg e) = ppr e
  ppr (ValArgQL { va_fun = fun, va_args = args, va_ty = ty})
    = hang (text "ValArgQL" <+> ppr fun)
         2 (vcat [ ppr args, text "va_ty:" <+> ppr ty ])

pprHsExprArgTc :: HsExprArg 'TcpInst -> SDoc
pprHsExprArgTc (EValArg { eva_arg = tm, eva_arg_ty = ty })
  = text "EValArg" <+> hang (ppr tm) 2 (dcolon <+> ppr ty)
pprHsExprArgTc arg = ppr arg

{- Note [Desugar OpApp in the typechecker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Operator sections are desugared in the renamer; see GHC.Rename.Expr
Note [Handling overloaded and rebindable constructs].
But for reasons explained there, we rename OpApp to OpApp.  Then,
here in the typechecker, we desugar it to a use of HsExpanded.
That makes it possible to typecheck something like
     e1 `f` e2
where
   f :: forall a. t1 -> forall b. t2 -> t3

Note [Looking through HsExpanded]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When creating an application chain in splitHsApps, we must deal with
     HsExpanded f1 (f `HsApp` e1) `HsApp` e2 `HsApp` e3

as a single application chain `f e1 e2 e3`.  Otherwise stuff like overloaded
labels (#19154) won't work.

It's easy to achieve this: `splitHsApps` unwraps `HsExpanded`.
-}

{- *********************************************************************
*                                                                      *
                 tcInferAppHead
*                                                                      *
********************************************************************* -}

tcInferAppHead :: (HsExpr GhcRn, AppCtxt)
               -> [HsExprArg 'TcpRn] -> Maybe TcRhoType
               -- These two args are solely for tcInferRecSelId
               -> TcM (HsExpr GhcTc, TcSigmaType)
-- Infer type of the head of an application
--   i.e. the 'f' in (f e1 ... en)
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
-- We get back a /SigmaType/ because we have special cases for
--   * A bare identifier (just look it up)
--     This case also covers a record selector HsRecFld
--   * An expression with a type signature (e :: ty)
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
--
-- Why do we need the arguments to infer the type of the head of
-- the application?  For two reasons:
--   * (Legitimate) The first arg has the source location of the head
--   * (Disgusting) Needed for record disambiguation; see tcInferRecSelId
--
-- Note that [] and (,,) are both HsVar:
--   see Note [Empty lists] and [ExplicitTuple] in GHC.Hs.Expr
--
-- NB: 'e' cannot be HsApp, HsTyApp, HsPrag, HsPar, because those
--     cases are dealt with by splitHsApps.
--
-- See Note [tcApp: typechecking applications] in GHC.Tc.Gen.App
tcInferAppHead (fun,ctxt) args mb_res_ty
  = setSrcSpan (appCtxtLoc ctxt) $
    do { mb_tc_fun <- tcInferAppHead_maybe fun args mb_res_ty
       ; case mb_tc_fun of
            Just (fun', fun_sigma) -> return (fun', fun_sigma)
            Nothing -> add_head_ctxt fun args $
                       tcInfer (tcExpr fun) }

tcInferAppHead_maybe :: HsExpr GhcRn
                     -> [HsExprArg 'TcpRn] -> Maybe TcRhoType
                        -- These two args are solely for tcInferRecSelId
                     -> TcM (Maybe (HsExpr GhcTc, TcSigmaType))
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
-- Returns Nothing for a complicated head
tcInferAppHead_maybe fun args mb_res_ty
  = case fun of
      HsVar _ (L _ nm)          -> Just <$> tcInferId nm
      HsRecFld _ f              -> Just <$> tcInferRecSelId f args mb_res_ty
      ExprWithTySig _ e hs_ty   -> add_head_ctxt fun args $
                                   Just <$> tcExprWithSig e hs_ty
      HsOverLit _ lit           -> Just <$> tcInferOverLit lit
      HsSpliceE _ (HsSpliced _ _ (HsSplicedExpr e))
                                -> tcInferAppHead_maybe e args mb_res_ty
      _                         -> return Nothing

add_head_ctxt :: HsExpr GhcRn -> [HsExprArg 'TcpRn] -> TcM a -> TcM a
-- Don't push an expression context if the arguments are empty,
-- because it has already been pushed by tcExpr
add_head_ctxt fun args thing_inside
  | null args = thing_inside
  | otherwise = addExprCtxt fun thing_inside


{- *********************************************************************
*                                                                      *
                 Record selectors
*                                                                      *
********************************************************************* -}

{-
Note [Deprecating ambiguous fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the future, the -XDuplicateRecordFields extension will no longer support
disambiguating record fields during type-checking (as described in Note
[Disambiguating record fields]).  For now, the -Wambiguous-fields option will
emit a warning whenever an ambiguous field is resolved using type information.
In a subsequent GHC release, this functionality will be removed and the warning
will turn into an ambiguity error in the renamer.

For background information, see GHC proposal #366
(https://github.com/ghc-proposals/ghc-proposals/pull/366).


Note [Disambiguating record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB. The following is going to be removed: see
Note [Deprecating ambiguous fields].

When the -XDuplicateRecordFields extension is used, and the renamer
encounters a record selector or update that it cannot immediately
disambiguate (because it involves fields that belong to multiple
datatypes), it will defer resolution of the ambiguity to the
typechecker.  In this case, the `Ambiguous` constructor of
`AmbiguousFieldOcc` is used.

Consider the following definitions:

        data S = MkS { foo :: Int }
        data T = MkT { foo :: Int, bar :: Int }
        data U = MkU { bar :: Int, baz :: Int }

When the renamer sees `foo` as a selector or an update, it will not
know which parent datatype is in use.

For selectors, there are two possible ways to disambiguate:

1. Check if the pushed-in type is a function whose domain is a
   datatype, for example:

       f s = (foo :: S -> Int) s

       g :: T -> Int
       g = foo

    This is checked by `tcCheckRecSelId` when checking `HsRecFld foo`.

2. Check if the selector is applied to an argument that has a type
   signature, for example:

       h = foo (s :: S)

    This is checked by `tcInferRecSelId`.


Updates are slightly more complex.  The `disambiguateRecordBinds`
function tries to determine the parent datatype in three ways:

1. Check for types that have all the fields being updated. For example:

        f x = x { foo = 3, bar = 2 }

   Here `f` must be updating `T` because neither `S` nor `U` have
   both fields. This may also discover that no possible type exists.
   For example the following will be rejected:

        f' x = x { foo = 3, baz = 3 }

2. Use the type being pushed in, if it is already a TyConApp. The
   following are valid updates to `T`:

        g :: T -> T
        g x = x { foo = 3 }

        g' x = x { foo = 3 } :: T

3. Use the type signature of the record expression, if it exists and
   is a TyConApp. Thus this is valid update to `T`:

        h x = (x :: T) { foo = 3 }


Note that we do not look up the types of variables being updated, and
no constraint-solving is performed, so for example the following will
be rejected as ambiguous:

     let bad (s :: S) = foo s

     let r :: T
         r = blah
     in r { foo = 3 }

     \r. (r { foo = 3 },  r :: T )

We could add further tests, of a more heuristic nature. For example,
rather than looking for an explicit signature, we could try to infer
the type of the argument to a selector or the record expression being
updated, in case we are lucky enough to get a TyConApp straight
away. However, it might be hard for programmers to predict whether a
particular update is sufficiently obvious for the signature to be
omitted. Moreover, this might change the behaviour of typechecker in
non-obvious ways.

See also Note [HsRecField and HsRecUpdField] in GHC.Hs.Pat.
-}

tcInferRecSelId :: AmbiguousFieldOcc GhcRn
                -> [HsExprArg 'TcpRn] -> Maybe TcRhoType
                -> TcM (HsExpr GhcTc, TcSigmaType)
tcInferRecSelId (Unambiguous sel_name lbl) _args _mb_res_ty
   = do { sel_id <- tc_rec_sel_id lbl sel_name
        ; let expr = HsRecFld noExtField (Unambiguous sel_id lbl)
        ; return (expr, idType sel_id) }

tcInferRecSelId (Ambiguous _ lbl) args mb_res_ty
   = do { sel_name <- tcInferAmbiguousRecSelId lbl args mb_res_ty
        ; sel_id   <- tc_rec_sel_id lbl sel_name
        ; let expr = HsRecFld noExtField (Ambiguous sel_id lbl)
        ; return (expr, idType sel_id) }

------------------------
tc_rec_sel_id :: LocatedN RdrName -> Name -> TcM TcId
-- Like tc_infer_id, but returns an Id not a HsExpr,
-- so we can wrap it back up into a HsRecFld
tc_rec_sel_id lbl sel_name
  = do { thing <- tcLookup sel_name
       ; case thing of
             ATcId { tct_id = id }
               -> do { check_naughty occ id
                     ; check_local_id id
                     ; return id }

             AGlobal (AnId id)
               -> do { check_naughty occ id
                     ; return id }
                    -- A global cannot possibly be ill-staged
                    -- nor does it need the 'lifting' treatment
                    -- hence no checkTh stuff here

             _ -> failWithTc $
                  ppr thing <+> text "used where a value identifier was expected" }
  where
    occ = rdrNameOcc (unLoc lbl)

------------------------
tcInferAmbiguousRecSelId :: LocatedN RdrName
                         -> [HsExprArg 'TcpRn] -> Maybe TcRhoType
                         -> TcM Name
-- Disgusting special case for ambiguous record selectors
-- Given a RdrName that refers to multiple record fields, and the type
-- of its argument, try to determine the name of the selector that is
-- meant.
-- See Note [Disambiguating record fields]
tcInferAmbiguousRecSelId lbl args mb_res_ty
  | arg1 : _ <- dropWhile (not . isVisibleArg) args -- A value arg is first
  , EValArg { eva_arg = ValArg (L _ arg) } <- arg1
  , Just sig_ty <- obviousSig arg  -- A type sig on the arg disambiguates
  = do { sig_tc_ty <- tcHsSigWcType ExprSigCtxt sig_ty
       ; finish_ambiguous_selector lbl sig_tc_ty }

  | Just res_ty <- mb_res_ty
  , Just (arg_ty,_) <- tcSplitFunTy_maybe res_ty
  = finish_ambiguous_selector lbl (scaledThing arg_ty)

  | otherwise
  = ambiguousSelector lbl

finish_ambiguous_selector :: LocatedN RdrName -> Type -> TcM Name
finish_ambiguous_selector lr@(L _ rdr) parent_type
 = do { fam_inst_envs <- tcGetFamInstEnvs
      ; case tyConOf fam_inst_envs parent_type of {
          Nothing -> ambiguousSelector lr ;
          Just p  ->

    do { xs <- lookupParents True rdr
       ; let parent = RecSelData p
       ; case lookup parent xs of {
           Nothing  -> failWithTc (fieldNotInType parent rdr) ;
           Just gre ->

    -- See Note [Unused name reporting and HasField] in GHC.Tc.Instance.Class
    do { addUsedGRE True gre
       ; keepAlive (greMangledName gre)
         -- See Note [Deprecating ambiguous fields]
       ; warnIfFlag Opt_WarnAmbiguousFields True $
          vcat [ text "The field" <+> quotes (ppr rdr)
                   <+> text "belonging to type" <+> ppr parent_type
                   <+> text "is ambiguous."
               , text "This will not be supported by -XDuplicateRecordFields in future releases of GHC."
               , if isLocalGRE gre
                 then text "You can use explicit case analysis to resolve the ambiguity."
                 else text "You can use a qualified import or explicit case analysis to resolve the ambiguity."
               ]
       ; return (greMangledName gre) } } } } }

-- This field name really is ambiguous, so add a suitable "ambiguous
-- occurrence" error, then give up.
ambiguousSelector :: LocatedN RdrName -> TcM a
ambiguousSelector (L _ rdr)
  = do { addAmbiguousNameErr rdr
       ; failM }

-- | This name really is ambiguous, so add a suitable "ambiguous
-- occurrence" error, then continue
addAmbiguousNameErr :: RdrName -> TcM ()
addAmbiguousNameErr rdr
  = do { env <- getGlobalRdrEnv
       ; let gres = lookupGRE_RdrName rdr env
       ; case gres of
         [] -> panic "addAmbiguousNameErr: not found"
         gre : gres -> setErrCtxt [] $ addNameClashErrRn rdr $ gre NE.:| gres}

-- A type signature on the argument of an ambiguous record selector or
-- the record expression in an update must be "obvious", i.e. the
-- outermost constructor ignoring parentheses.
obviousSig :: HsExpr GhcRn -> Maybe (LHsSigWcType GhcRn)
obviousSig (ExprWithTySig _ _ ty) = Just ty
obviousSig (HsPar _ p)            = obviousSig (unLoc p)
obviousSig (HsPragE _ _ p)        = obviousSig (unLoc p)
obviousSig _                      = Nothing

-- Extract the outermost TyCon of a type, if there is one; for
-- data families this is the representation tycon (because that's
-- where the fields live).
tyConOf :: FamInstEnvs -> TcSigmaType -> Maybe TyCon
tyConOf fam_inst_envs ty0
  = case tcSplitTyConApp_maybe ty of
      Just (tc, tys) -> Just (fstOf3 (tcLookupDataFamInst fam_inst_envs tc tys))
      Nothing        -> Nothing
  where
    (_, _, ty) = tcSplitSigmaTy ty0

-- Variant of tyConOf that works for ExpTypes
tyConOfET :: FamInstEnvs -> ExpRhoType -> Maybe TyCon
tyConOfET fam_inst_envs ty0 = tyConOf fam_inst_envs =<< checkingExpType_maybe ty0


-- For an ambiguous record field, find all the candidate record
-- selectors (as GlobalRdrElts) and their parents.
lookupParents :: Bool -> RdrName -> RnM [(RecSelParent, GlobalRdrElt)]
lookupParents is_selector rdr
  = do { env <- getGlobalRdrEnv
        -- Filter by isRecFldGRE because otherwise a non-selector variable with
        -- an overlapping name can get through when NoFieldSelectors is enabled.
        -- See Note [NoFieldSelectors] in GHC.Rename.Env.
       ; let all_gres = lookupGRE_RdrName' rdr env
       ; let gres | is_selector = filter isFieldSelectorGRE all_gres
                  | otherwise   = filter isRecFldGRE all_gres
       ; mapM lookupParent gres }
  where
    lookupParent :: GlobalRdrElt -> RnM (RecSelParent, GlobalRdrElt)
    lookupParent gre = do { id <- tcLookupId (greMangledName gre)
                          ; case recordSelectorTyCon_maybe id of
                              Just rstc -> return (rstc, gre)
                              Nothing -> failWithTc (notSelector (greMangledName gre)) }


fieldNotInType :: RecSelParent -> RdrName -> SDoc
fieldNotInType p rdr
  = unknownSubordinateErr (text "field of type" <+> quotes (ppr p)) rdr

notSelector :: Name -> SDoc
notSelector field
  = hsep [quotes (ppr field), text "is not a record selector"]

naughtyRecordSel :: OccName -> SDoc
naughtyRecordSel lbl
  = text "Cannot use record selector" <+> quotes (ppr lbl) <+>
    text "as a function due to escaped type variables" $$
    text "Probable fix: use pattern-matching syntax instead"


{- *********************************************************************
*                                                                      *
                Expressions with a type signature
                        expr :: type
*                                                                      *
********************************************************************* -}

tcExprWithSig :: LHsExpr GhcRn -> LHsSigWcType (NoGhcTc GhcRn)
              -> TcM (HsExpr GhcTc, TcSigmaType)
tcExprWithSig expr hs_ty
  = do { sig_info <- checkNoErrs $  -- Avoid error cascade
                     tcUserTypeSig loc hs_ty Nothing
       ; (expr', poly_ty) <- tcExprSig expr sig_info
       ; return (ExprWithTySig noExtField expr' hs_ty, poly_ty) }
  where
    loc = getLocA (dropWildCards hs_ty)

tcExprSig :: LHsExpr GhcRn -> TcIdSigInfo -> TcM (LHsExpr GhcTc, TcType)
tcExprSig expr (CompleteSig { sig_bndr = poly_id, sig_loc = loc })
  = setSrcSpan loc $   -- Sets the location for the implication constraint
    do { let poly_ty = idType poly_id
       ; (wrap, expr') <- tcSkolemiseScoped ExprSigCtxt poly_ty $ \rho_ty ->
                          tcCheckMonoExprNC expr rho_ty
       ; return (mkLHsWrap wrap expr', poly_ty) }

tcExprSig expr sig@(PartialSig { psig_name = name, sig_loc = loc })
  = setSrcSpan loc $   -- Sets the location for the implication constraint
    do { (tclvl, wanted, (expr', sig_inst))
             <- pushLevelAndCaptureConstraints  $
                do { sig_inst <- tcInstSig sig
                   ; expr' <- tcExtendNameTyVarEnv (mapSnd binderVar $ sig_inst_skols sig_inst) $
                              tcExtendNameTyVarEnv (sig_inst_wcs   sig_inst) $
                              tcCheckPolyExprNC expr (sig_inst_tau sig_inst)
                   ; return (expr', sig_inst) }
       -- See Note [Partial expression signatures]
       ; let tau = sig_inst_tau sig_inst
             infer_mode | null (sig_inst_theta sig_inst)
                        , isNothing (sig_inst_wcx sig_inst)
                        = ApplyMR
                        | otherwise
                        = NoRestrictions
       ; (qtvs, givens, ev_binds, _)
                 <- simplifyInfer tclvl infer_mode [sig_inst] [(name, tau)] wanted

       ; tau <- zonkTcType tau
       ; let inferred_theta = map evVarPred givens
             tau_tvs        = tyCoVarsOfType tau
       ; (binders, my_theta) <- chooseInferredQuantifiers inferred_theta
                                   tau_tvs qtvs (Just sig_inst)
       ; let inferred_sigma = mkInfSigmaTy qtvs inferred_theta tau
             my_sigma       = mkInvisForAllTys binders (mkPhiTy  my_theta tau)
       ; wrap <- if inferred_sigma `eqType` my_sigma -- NB: eqType ignores vis.
                 then return idHsWrapper  -- Fast path; also avoids complaint when we infer
                                          -- an ambiguous type and have AllowAmbiguousType
                                          -- e..g infer  x :: forall a. F a -> Int
                 else tcSubTypeSigma ExprSigCtxt inferred_sigma my_sigma

       ; traceTc "tcExpSig" (ppr qtvs $$ ppr givens $$ ppr inferred_sigma $$ ppr my_sigma)
       ; let poly_wrap = wrap
                         <.> mkWpTyLams qtvs
                         <.> mkWpLams givens
                         <.> mkWpLet  ev_binds
       ; return (mkLHsWrap poly_wrap expr', my_sigma) }


{- Note [Partial expression signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Partial type signatures on expressions are easy to get wrong.  But
here is a guiding principile
    e :: ty
should behave like
    let x :: ty
        x = e
    in x

So for partial signatures we apply the MR if no context is given.  So
   e :: IO _          apply the MR
   e :: _ => IO _     do not apply the MR
just like in GHC.Tc.Gen.Bind.decideGeneralisationPlan

This makes a difference (#11670):
   peek :: Ptr a -> IO CLong
   peek ptr = peekElemOff undefined 0 :: _
from (peekElemOff undefined 0) we get
          type: IO w
   constraints: Storable w

We must NOT try to generalise over 'w' because the signature specifies
no constraints so we'll complain about not being able to solve
Storable w.  Instead, don't generalise; then _ gets instantiated to
CLong, as it should.
-}


{- *********************************************************************
*                                                                      *
                 Overloaded literals
*                                                                      *
********************************************************************* -}

tcInferOverLit :: HsOverLit GhcRn -> TcM (HsExpr GhcTc, TcSigmaType)
tcInferOverLit lit@(OverLit { ol_val = val
                            , ol_witness = HsVar _ (L loc from_name)
                            , ol_ext = rebindable })
  = -- Desugar "3" to (fromInteger (3 :: Integer))
    --   where fromInteger is gotten by looking up from_name, and
    --   the (3 :: Integer) is returned by mkOverLit
    -- Ditto the string literal "foo" to (fromString ("foo" :: String))
    do { from_id <- tcLookupId from_name
       ; (wrap1, from_ty) <- topInstantiate orig (idType from_id)

       ; (wrap2, sarg_ty, res_ty) <- matchActualFunTySigma herald mb_doc
                                                           (1, []) from_ty
       ; hs_lit <- mkOverLit val
       ; co <- unifyType mb_doc (hsLitType hs_lit) (scaledThing sarg_ty)

       ; let lit_expr = L (l2l loc) $ mkHsWrapCo co $
                        HsLit noAnn hs_lit
             from_expr = mkHsWrap (wrap2 <.> wrap1) $
                         HsVar noExtField (L loc from_id)
             lit' = lit { ol_witness = HsApp noAnn (L (l2l loc) from_expr) lit_expr
                        , ol_ext = OverLitTc rebindable res_ty }
       ; return (HsOverLit noAnn lit', res_ty) }
  where
    orig   = LiteralOrigin lit
    mb_doc = Just (ppr from_name)
    herald = sep [ text "The function" <+> quotes (ppr from_name)
                 , text "is applied to"]

tcInferOverLit lit
  = pprPanic "tcInferOverLit" (ppr lit)


{- *********************************************************************
*                                                                      *
                 tcInferId, tcCheckId
*                                                                      *
********************************************************************* -}

tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr GhcTc)
tcCheckId name res_ty
  = do { (expr, actual_res_ty) <- tcInferId name
       ; traceTc "tcCheckId" (vcat [ppr name, ppr actual_res_ty, ppr res_ty])
       ; addFunResCtxt rn_fun [] actual_res_ty res_ty $
         tcWrapResultO (OccurrenceOf name) rn_fun expr actual_res_ty res_ty }
  where
    rn_fun = HsVar noExtField (noLocA name)

------------------------
tcInferId :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
-- Look up an occurrence of an Id
-- Do not instantiate its type
tcInferId id_name
  | id_name `hasKey` assertIdKey
  = do { dflags <- getDynFlags
       ; if gopt Opt_IgnoreAsserts dflags
         then tc_infer_id id_name
         else tc_infer_assert id_name }

  | otherwise
  = do { (expr, ty) <- tc_infer_id id_name
       ; traceTc "tcInferId" (ppr id_name <+> dcolon <+> ppr ty)
       ; return (expr, ty) }

tc_infer_assert :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
-- Deal with an occurrence of 'assert'
-- See Note [Adding the implicit parameter to 'assert']
tc_infer_assert assert_name
  = do { assert_error_id <- tcLookupId assertErrorName
       ; (wrap, id_rho) <- topInstantiate (OccurrenceOf assert_name)
                                          (idType assert_error_id)
       ; return (mkHsWrap wrap (HsVar noExtField (noLocA assert_error_id)), id_rho)
       }

tc_infer_id :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
tc_infer_id id_name
 = do { thing <- tcLookup id_name
      ; global_env <- getGlobalRdrEnv
      ; case thing of
             ATcId { tct_id = id }
               -> do { check_local_id id
                     ; return_id id }

             AGlobal (AnId id)
               -> return_id id
               -- A global cannot possibly be ill-staged
               -- nor does it need the 'lifting' treatment
               -- Hence no checkTh stuff here

             AGlobal (AConLike cl) -> case cl of
                 RealDataCon con -> return_data_con con
                 PatSynCon ps
                   | Just (expr, ty) <- patSynBuilderOcc ps
                   -> return (expr, ty)
                   | otherwise
                   -> failWithTc (nonBidirectionalErr id_name)

             AGlobal (ATyCon ty_con)
               -> fail_tycon global_env ty_con

             ATyVar name _
                -> failWithTc $
                     text "Illegal term-level use of the type variable"
                       <+> quotes (ppr name)
                       $$ nest 2 (text "bound at" <+> ppr (getSrcLoc name))

             ATcTyCon ty_con
               -> fail_tycon global_env ty_con

             _ -> failWithTc $
                  ppr thing <+> text "used where a value identifier was expected" }
  where
    fail_tycon global_env ty_con =
      let pprov = case lookupGRE_Name global_env (tyConName ty_con) of
            Just gre -> nest 2 (pprNameProvenance gre)
            Nothing  -> empty
      in failWithTc (term_level_tycons ty_con $$ pprov)

    term_level_tycons ty_con
      = text "Illegal term-level use of the type constructor"
          <+> quotes (ppr (tyConName ty_con))

    return_id id = return (HsVar noExtField (noLocA id), idType id)

    return_data_con con
      = do { let tvs = dataConUserTyVarBinders con
                 theta = dataConOtherTheta con
                 args = dataConOrigArgTys con
                 res = dataConOrigResTy con

           -- See Note [Linear fields generalization]
           ; mul_vars <- newFlexiTyVarTys (length args) multiplicityTy
           ; let scaleArgs args' = zipWithEqual "return_data_con" combine mul_vars args'
                 combine var (Scaled One ty) = Scaled var ty
                 combine _   scaled_ty       = scaled_ty
                   -- The combine function implements the fact that, as
                   -- described in Note [Linear fields generalization], if a
                   -- field is not linear (last line) it isn't made polymorphic.

                 etaWrapper arg_tys = foldr (\scaled_ty wr -> WpFun WpHole wr scaled_ty empty) WpHole arg_tys

           -- See Note [Instantiating stupid theta]
           ; let shouldInstantiate = (not (null (dataConStupidTheta con)) ||
                                      isKindLevPoly (tyConResKind (dataConTyCon con)))
           ; case shouldInstantiate of
               True -> do { (subst, tvs') <- newMetaTyVars (binderVars tvs)
                           ; let tys'   = mkTyVarTys tvs'
                                 theta' = substTheta subst theta
                                 args'  = substScaledTys subst args
                                 res'   = substTy subst res
                           ; wrap <- instCall (OccurrenceOf id_name) tys' theta'
                           ; let scaled_arg_tys = scaleArgs args'
                                 eta_wrap = etaWrapper scaled_arg_tys
                           ; addDataConStupidTheta con tys'
                           ; return ( mkHsWrap (eta_wrap <.> wrap)
                                               (HsConLikeOut noExtField (RealDataCon con))
                                    , mkVisFunTys scaled_arg_tys res')
                           }
               False -> let scaled_arg_tys = scaleArgs args
                            wrap1 = mkWpTyApps (mkTyVarTys $ binderVars tvs)
                            eta_wrap = etaWrapper (map unrestricted theta ++ scaled_arg_tys)
                            wrap2 = mkWpTyLams $ binderVars tvs
                        in return ( mkHsWrap (wrap2 <.> eta_wrap <.> wrap1)
                                             (HsConLikeOut noExtField (RealDataCon con))
                                  , mkInvisForAllTys tvs $ mkInvisFunTysMany theta $ mkVisFunTys scaled_arg_tys res)
           }

check_local_id :: Id -> TcM ()
check_local_id id
  = do { checkThLocalId id
       ; tcEmitBindingUsage $ unitUE (idName id) One }

check_naughty :: OccName -> TcId -> TcM ()
check_naughty lbl id
  | isNaughtyRecordSelector id = failWithTc (naughtyRecordSel lbl)
  | otherwise                  = return ()

nonBidirectionalErr :: Outputable name => name -> SDoc
nonBidirectionalErr name = text "non-bidirectional pattern synonym"
                           <+> quotes (ppr name) <+> text "used in an expression"

{-
Note [Linear fields generalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As per Note [Polymorphisation of linear fields], linear field of data
constructors get a polymorphic type when the data constructor is used as a term.

    Just :: forall {p} a. a #p-> Maybe a

This rule is known only to the typechecker: Just keeps its linear type in Core.

In order to desugar this generalised typing rule, we simply eta-expand:

    \a (x # p :: a) -> Just @a x

has the appropriate type. We insert these eta-expansion with WpFun wrappers.

A small hitch: if the constructor is levity-polymorphic (unboxed tuples, sums,
certain newtypes with -XUnliftedNewtypes) then this strategy produces

    \r1 r2 a b (x # p :: a) (y # q :: b) -> (# a, b #)

Which has type

    forall r1 r2 a b. a #p-> b #q-> (# a, b #)

Which violates the levity-polymorphism restriction see Note [Levity polymorphism
checking] in DsMonad.

So we really must instantiate r1 and r2 rather than quantify over them.  For
simplicity, we just instantiate the entire type, as described in Note
[Instantiating stupid theta]. It breaks visible type application with unboxed
tuples, sums and levity-polymorphic newtypes, but this doesn't appear to be used
anywhere.

A better plan: let's force all representation variable to be *inferred*, so that
they are not subject to visible type applications. Then we can instantiate
inferred argument eagerly.

Note [Adding the implicit parameter to 'assert']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker transforms (assert e1 e2) to (assertError e1 e2).
This isn't really the Right Thing because there's no way to "undo"
if you want to see the original source code in the typechecker
output.  We'll have fix this in due course, when we care more about
being able to reconstruct the exact original program.


Note [Instantiating stupid theta]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, when we infer the type of an Id, we don't instantiate,
because we wish to allow for visible type application later on.
But if a datacon has a stupid theta, we're a bit stuck. We need
to emit the stupid theta constraints with instantiated types. It's
difficult to defer this to the lazy instantiation, because a stupid
theta has no spot to put it in a type. So we just instantiate eagerly
in this case. Thus, users cannot use visible type application with
a data constructor sporting a stupid theta. I won't feel so bad for
the users that complain.
-}

{-
************************************************************************
*                                                                      *
                 Template Haskell checks
*                                                                      *
************************************************************************
-}

checkThLocalId :: Id -> TcM ()
-- The renamer has already done checkWellStaged,
--   in RnSplice.checkThLocalName, so don't repeat that here.
-- Here we just add constraints for cross-stage lifting
checkThLocalId id
  = do  { mb_local_use <- getStageAndBindLevel (idName id)
        ; case mb_local_use of
             Just (top_lvl, bind_lvl, use_stage)
                | thLevel use_stage > bind_lvl
                -> checkCrossStageLifting top_lvl id use_stage
             _  -> return ()   -- Not a locally-bound thing, or
                               -- no cross-stage link
    }

--------------------------------------
checkCrossStageLifting :: TopLevelFlag -> Id -> ThStage -> TcM ()
-- If we are inside typed brackets, and (use_lvl > bind_lvl)
-- we must check whether there's a cross-stage lift to do
-- Examples   \x -> [|| x ||]
--            [|| map ||]
--
-- This is similar to checkCrossStageLifting in GHC.Rename.Splice, but
-- this code is applied to *typed* brackets.

checkCrossStageLifting top_lvl id (Brack _ (TcPending ps_var lie_var q))
  | isTopLevel top_lvl
  = when (isExternalName id_name) (keepAlive id_name)
    -- See Note [Keeping things alive for Template Haskell] in GHC.Rename.Splice

  | otherwise
  =     -- Nested identifiers, such as 'x' in
        -- E.g. \x -> [|| h x ||]
        -- We must behave as if the reference to x was
        --      h $(lift x)
        -- We use 'x' itself as the splice proxy, used by
        -- the desugarer to stitch it all back together.
        -- If 'x' occurs many times we may get many identical
        -- bindings of the same splice proxy, but that doesn't
        -- matter, although it's a mite untidy.
    do  { let id_ty = idType id
        ; checkTc (isTauTy id_ty) (polySpliceErr id)
               -- If x is polymorphic, its occurrence sites might
               -- have different instantiations, so we can't use plain
               -- 'x' as the splice proxy name.  I don't know how to
               -- solve this, and it's probably unimportant, so I'm
               -- just going to flag an error for now

        ; lift <- if isStringTy id_ty then
                     do { sid <- tcLookupId GHC.Builtin.Names.TH.liftStringName
                                     -- See Note [Lifting strings]
                        ; return (HsVar noExtField (noLocA sid)) }
                  else
                     setConstraintVar lie_var   $
                          -- Put the 'lift' constraint into the right LIE
                     newMethodFromName (OccurrenceOf id_name)
                                       GHC.Builtin.Names.TH.liftName
                                       [getRuntimeRep id_ty, id_ty]

                   -- Warning for implicit lift (#17804)
        ; whenWOptM Opt_WarnImplicitLift $
            addWarnTc (Reason Opt_WarnImplicitLift)
                       (text "The variable" <+> quotes (ppr id) <+>
                        text "is implicitly lifted in the TH quotation")

                   -- Update the pending splices
        ; ps <- readMutVar ps_var
        ; let pending_splice = PendingTcSplice id_name
                                 (nlHsApp (mkLHsWrap (applyQuoteWrapper q) (noLocA lift))
                                          (nlHsVar id))
        ; writeMutVar ps_var (pending_splice : ps)

        ; return () }
  where
    id_name = idName id

checkCrossStageLifting _ _ _ = return ()

polySpliceErr :: Id -> SDoc
polySpliceErr id
  = text "Can't splice the polymorphic local variable" <+> quotes (ppr id)

{-
Note [Lifting strings]
~~~~~~~~~~~~~~~~~~~~~~
If we see $(... [| s |] ...) where s::String, we don't want to
generate a mass of Cons (CharL 'x') (Cons (CharL 'y') ...)) etc.
So this conditional short-circuits the lifting mechanism to generate
(liftString "xy") in that case.  I didn't want to use overlapping instances
for the Lift class in TH.Syntax, because that can lead to overlapping-instance
errors in a polymorphic situation.

If this check fails (which isn't impossible) we get another chance; see
Note [Converting strings] in Convert.hs

Local record selectors
~~~~~~~~~~~~~~~~~~~~~~
Record selectors for TyCons in this module are ordinary local bindings,
which show up as ATcIds rather than AGlobals.  So we need to check for
naughtiness in both branches.  c.f. TcTyClsBindings.mkAuxBinds.
-}


{- *********************************************************************
*                                                                      *
         Error reporting for function result mis-matches
*                                                                      *
********************************************************************* -}

addFunResCtxt :: HsExpr GhcRn -> [HsExprArg 'TcpRn]
              -> TcType -> ExpRhoType
              -> TcM a -> TcM a
-- When we have a mis-match in the return type of a function
-- try to give a helpful message about too many/few arguments
-- But not in generated code, where we don't want
-- to mention internal (rebindable syntax) function names
addFunResCtxt fun args fun_res_ty env_ty thing_inside
  = addLandmarkErrCtxtM (\env -> (env, ) <$> mk_msg) thing_inside
      -- NB: use a landmark error context, so that an empty context
      -- doesn't suppress some more useful context
  where
    mk_msg
      = do { mb_env_ty <- readExpType_maybe env_ty
                     -- by the time the message is rendered, the ExpType
                     -- will be filled in (except if we're debugging)
           ; fun_res' <- zonkTcType fun_res_ty
           ; env'     <- case mb_env_ty of
                           Just env_ty -> zonkTcType env_ty
                           Nothing     ->
                             do { dumping <- doptM Opt_D_dump_tc_trace
                                ; MASSERT( dumping )
                                ; newFlexiTyVarTy liftedTypeKind }
           ; let -- See Note [Splitting nested sigma types in mismatched
                 --           function types]
                 (_, _, fun_tau) = tcSplitNestedSigmaTys fun_res'
                 -- No need to call tcSplitNestedSigmaTys here, since env_ty is
                 -- an ExpRhoTy, i.e., it's already instantiated.
                 (_, _, env_tau) = tcSplitSigmaTy env'
                 (args_fun, res_fun) = tcSplitFunTys fun_tau
                 (args_env, res_env) = tcSplitFunTys env_tau
                 n_fun = length args_fun
                 n_env = length args_env
                 info  | -- Check for too few args
                         --  fun_tau = a -> b, res_tau = Int
                         n_fun > n_env
                       , not_fun res_env
                       = text "Probable cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too few arguments"

                       | -- Check for too many args
                         -- fun_tau = a -> Int,   res_tau = a -> b -> c -> d
                         -- The final guard suppresses the message when there
                         -- aren't enough args to drop; eg. the call is (f e1)
                         n_fun < n_env
                       , not_fun res_fun
                       , (n_fun + count isValArg args) >= n_env
                          -- Never suggest that a naked variable is
                                           -- applied to too many args!
                       = text "Possible cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too many arguments"

                       | otherwise
                       = Outputable.empty

           ; return info }

    not_fun ty   -- ty is definitely not an arrow type,
                 -- and cannot conceivably become one
      = case tcSplitTyConApp_maybe ty of
          Just (tc, _) -> isAlgTyCon tc
          Nothing      -> False

{-
Note [Splitting nested sigma types in mismatched function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When one applies a function to too few arguments, GHC tries to determine this
fact if possible so that it may give a helpful error message. It accomplishes
this by checking if the type of the applied function has more argument types
than supplied arguments.

Previously, GHC computed the number of argument types through tcSplitSigmaTy.
This is incorrect in the face of nested foralls, however!
This caused Ticket #13311, for instance:

  f :: forall a. (Monoid a) => forall b. (Monoid b) => Maybe a -> Maybe b

If one uses `f` like so:

  do { f; putChar 'a' }

Then tcSplitSigmaTy will decompose the type of `f` into:

  Tyvars: [a]
  Context: (Monoid a)
  Argument types: []
  Return type: forall b. Monoid b => Maybe a -> Maybe b

That is, it will conclude that there are *no* argument types, and since `f`
was given no arguments, it won't print a helpful error message. On the other
hand, tcSplitNestedSigmaTys correctly decomposes `f`'s type down to:

  Tyvars: [a, b]
  Context: (Monoid a, Monoid b)
  Argument types: [Maybe a]
  Return type: Maybe b

So now GHC recognizes that `f` has one more argument type than it was actually
provided.
-}


{- *********************************************************************
*                                                                      *
             Misc utility functions
*                                                                      *
********************************************************************* -}

addExprCtxt :: HsExpr GhcRn -> TcRn a -> TcRn a
addExprCtxt e thing_inside
  = case e of
      HsUnboundVar {} -> thing_inside
      _ -> addErrCtxt (exprCtxt e) thing_inside
   -- The HsUnboundVar special case addresses situations like
   --    f x = _
   -- when we don't want to say "In the expression: _",
   -- because it is mentioned in the error message itself

exprCtxt :: HsExpr GhcRn -> SDoc
exprCtxt expr = hang (text "In the expression:") 2 (ppr (stripParensHsExpr expr))
