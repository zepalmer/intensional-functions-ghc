{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


This module contains definitions for the IdInfo for things that
have a standard form, namely:

- data constructors
- record selectors
- method and superclass selectors
- primitive operations
-}



{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Types.Id.Make (
        mkDictFunId, mkDictSelId, mkDictSelRhs,

        mkFCallId,

        unwrapNewTypeBody, wrapFamInstBody,
        DataConBoxer(..), vanillaDataConBoxer,
        mkDataConRep, mkDataConWorkId,
        DataConBangOpts (..), BangOpts (..),
        unboxedUnitExpr,

        -- And some particular Ids; see below for why they are wired in
        wiredInIds, ghcPrimIds,
        realWorldPrimId,
        voidPrimId, voidArgId,
        nullAddrId, seqId, lazyId, lazyIdKey,
        coercionTokenId, coerceId,
        proxyHashId,
        nospecId, nospecIdName,
        noinlineId, noinlineIdName,
        noinlineConstraintId, noinlineConstraintIdName,
        coerceName, leftSectionName, rightSectionName,
    ) where

import GHC.Prelude

import GHC.Builtin.Types.Prim
import GHC.Builtin.Types
import GHC.Builtin.Names

import GHC.Core
import GHC.Core.Opt.Arity( typeOneShot )
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.TyCo.Rep
import GHC.Core.FamInstEnv
import GHC.Core.Coercion
import GHC.Core.Reduction
import GHC.Core.Make
import GHC.Core.FVs     ( mkRuleInfo )
import GHC.Core.Utils   ( exprType, mkCast, mkDefaultCase, coreAltsType )
import GHC.Core.Unfold.Make
import GHC.Core.SimpleOpt
import GHC.Core.TyCon
import GHC.Core.Class
import GHC.Core.DataCon

import GHC.Types.Literal
import GHC.Types.SourceText
import GHC.Types.Name.Set
import GHC.Types.Name
import GHC.Types.ForeignCall
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Types.Unique.Supply
import GHC.Types.Basic       hiding ( SuccessFlag(..) )
import GHC.Types.Var (VarBndr(Bndr), visArgConstraintLike)

import GHC.Tc.Utils.TcType as TcType

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

import GHC.Data.FastString
import GHC.Data.List.SetOps
import Data.List        ( zipWith4 )

{-
************************************************************************
*                                                                      *
\subsection{Wired in Ids}
*                                                                      *
************************************************************************

Note [Wired-in Ids]
~~~~~~~~~~~~~~~~~~~
A "wired-in" Id can be referred to directly in GHC (e.g. 'voidPrimId')
rather than by looking it up its name in some environment or fetching
it from an interface file.

There are several reasons why an Id might appear in the wiredInIds:

* ghcPrimIds: see Note [ghcPrimIds (aka pseudoops)]

* magicIds: see Note [magicIds]

* errorIds, defined in GHC.Core.Make.
  These error functions (e.g. rUNTIME_ERROR_ID) are wired in
  because the desugarer generates code that mentions them directly

In all cases except ghcPrimIds, there is a definition site in a
library module, which may be called (e.g. in higher order situations);
but the wired-in version means that the details are never read from
that module's interface file; instead, the full definition is right
here.

Note [ghcPrimIds (aka pseudoops)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ghcPrimIds

  * Are exported from GHC.Prim (see ghcPrimExports, used in ghcPrimInterface)
    See Note [GHC.Prim] in primops.txt.pp for the remaining items in GHC.Prim.

  * Can't be defined in Haskell, and hence no Haskell binding site,
    but have perfectly reasonable unfoldings in Core

  * Either have a CompulsoryUnfolding (hence always inlined), or
        of an EvaldUnfolding and void representation (e.g. realWorldPrimId)

  * Are (or should be) defined in primops.txt.pp as 'pseudoop'
    Reason: that's how we generate documentation for them

Note [magicIds]
~~~~~~~~~~~~~~~
The magicIds

  * Are exported from GHC.Magic

  * Can be defined in Haskell (and are, in ghc-prim:GHC/Magic.hs).
    This definition at least generates Haddock documentation for them.

  * May or may not have a CompulsoryUnfolding.

  * But have some special behaviour that can't be done via an
    unfolding from an interface file.

  * May have IdInfo that differs from what would be imported from GHC.Magic.hi.
    For example, 'lazy' gets a lazy strictness signature, per Note [lazyId magic].

  The two remaining identifiers in GHC.Magic, runRW# and inline, are not listed
  in magicIds: they have special behavior but they can be known-key and
  not wired-in.
  runRW#: see Note [Simplification of runRW#] in Prep, runRW# code in
  Simplifier, Note [Linting of runRW#].
  inline: see Note [inlineId magic]
-}

wiredInIds :: [Id]
wiredInIds
  =  magicIds
  ++ ghcPrimIds
  ++ errorIds           -- Defined in GHC.Core.Make

magicIds :: [Id]    -- See Note [magicIds]
magicIds = [lazyId, oneShotId, noinlineId, noinlineConstraintId, nospecId]

ghcPrimIds :: [Id]  -- See Note [ghcPrimIds (aka pseudoops)]
ghcPrimIds
  = [ realWorldPrimId
    , voidPrimId
    , nullAddrId
    , seqId
    , coerceId
    , proxyHashId
    , leftSectionId
    , rightSectionId
    ]

{-
************************************************************************
*                                                                      *
\subsection{Data constructors}
*                                                                      *
************************************************************************

The wrapper for a constructor is an ordinary top-level binding that evaluates
any strict args, unboxes any args that are going to be flattened, and calls
the worker.

We're going to build a constructor that looks like:

        data (Data a, C b) =>  T a b = T1 !a !Int b

        T1 = /\ a b ->
             \d1::Data a, d2::C b ->
             \p q r -> case p of { p ->
                       case q of { q ->
                       Con T1 [a,b] [p,q,r]}}

Notice that

* d2 is thrown away --- a context in a data decl is used to make sure
  one *could* construct dictionaries at the site the constructor
  is used, but the dictionary isn't actually used.

* We have to check that we can construct Data dictionaries for
  the types a and Int.  Once we've done that we can throw d1 away too.

* We use (case p of q -> ...) to evaluate p, rather than "seq" because
  all that matters is that the arguments are evaluated.  "seq" is
  very careful to preserve evaluation order, which we don't need
  to be here.

  You might think that we could simply give constructors some strictness
  info, like PrimOps, and let CoreToStg do the let-to-case transformation.
  But we don't do that because in the case of primops and functions strictness
  is a *property* not a *requirement*.  In the case of constructors we need to
  do something active to evaluate the argument.

  Making an explicit case expression allows the simplifier to eliminate
  it in the (common) case where the constructor arg is already evaluated.

Note [Wrappers for data instance tycons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the case of data instances, the wrapper also applies the coercion turning
the representation type into the family instance type to cast the result of
the wrapper.  For example, consider the declarations

  data family Map k :: * -> *
  data instance Map (a, b) v = MapPair (Map a (Pair b v))

The tycon to which the datacon MapPair belongs gets a unique internal
name of the form :R123Map, and we call it the representation tycon.
In contrast, Map is the family tycon (accessible via
tyConFamInst_maybe). A coercion allows you to move between
representation and family type.  It is accessible from :R123Map via
tyConFamilyCoercion_maybe and has kind

  Co123Map a b v :: {Map (a, b) v ~ :R123Map a b v}

The wrapper and worker of MapPair get the types

        -- Wrapper
  $WMapPair :: forall a b v. Map a (Map a b v) -> Map (a, b) v
  $WMapPair a b v = MapPair a b v `cast` sym (Co123Map a b v)

        -- Worker
  MapPair :: forall a b v. Map a (Map a b v) -> :R123Map a b v

This coercion is conditionally applied by wrapFamInstBody.

It's a bit more complicated if the data instance is a GADT as well!

   data instance T [a] where
        T1 :: forall b. b -> T [Maybe b]

Hence we translate to

        -- Wrapper
  $WT1 :: forall b. b -> T [Maybe b]
  $WT1 b v = T1 (Maybe b) b (Maybe b) v
                        `cast` sym (Co7T (Maybe b))

        -- Worker
  T1 :: forall c b. (c ~ Maybe b) => b -> :R7T c

        -- Coercion from family type to representation type
  Co7T a :: T [a] ~ :R7T a

Newtype instances through an additional wrinkle into the mix. Consider the
following example (adapted from #15318, comment:2):

  data family T a
  newtype instance T [a] = MkT [a]

Within the newtype instance, there are three distinct types at play:

1. The newtype's underlying type, [a].
2. The instance's representation type, TList a (where TList is the
   representation tycon).
3. The family type, T [a].

We need two coercions in order to cast from (1) to (3):

(a) A newtype coercion axiom:

      axiom coTList a :: TList a ~ [a]

    (Where TList is the representation tycon of the newtype instance.)

(b) A data family instance coercion axiom:

      axiom coT a :: T [a] ~ TList a

When we translate the newtype instance to Core, we obtain:

    -- Wrapper
  $WMkT :: forall a. [a] -> T [a]
  $WMkT a x = MkT a x |> Sym (coT a)

    -- Worker
  MkT :: forall a. [a] -> TList [a]
  MkT a x = x |> Sym (coTList a)

Unlike for data instances, the worker for a newtype instance is actually an
executable function which expands to a cast, but otherwise, the general
strategy is essentially the same as for data instances. Also note that we have
a wrapper, which is unusual for a newtype, but we make GHC produce one anyway
for symmetry with the way data instances are handled.

Note [Newtype datacons]
~~~~~~~~~~~~~~~~~~~~~~~
The "data constructor" for a newtype should have no existentials. It's
not quite a "vanilla" data constructor, because the newtype arising from
     class C a => D a
looks like
       newtype T:D a = C:D (C a)
so the data constructor for T:C has a single argument, namely the
predicate (C a).  That ends up in the dcOtherTheta for the data con,
which makes it not vanilla.  So the assert just tests for existentials.
The rest is checked by having a singleton arg_tys.

Note [Newtype workers]
~~~~~~~~~~~~~~~~~~~~~~
A newtype does not really have a worker. Instead, newtype constructors
just unfold into a cast. But we need *something* for, say, MkAge to refer
to. So, we do this:

* The Id used as the newtype worker will have a compulsory unfolding to
  a cast. See Note [Compulsory newtype unfolding]

* This Id is labeled as a DataConWrapId. We don't want to use a DataConWorkId,
  as those have special treatment in the back end.

* There is no top-level binding, because the compulsory unfolding
  means that it will be inlined (to a cast) at every call site.

We probably should have a NewtypeWorkId, but these Ids disappear as soon as
we desugar anyway, so it seems a step too far.

Note [Compulsory newtype unfolding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Newtype wrappers, just like workers, have compulsory unfoldings.
This is needed so that two optimizations involving newtypes have the same
effect whether a wrapper is present or not:

(1) Case-of-known constructor.
    See Note [beta-reduction in exprIsConApp_maybe].

(2) Matching against the map/coerce RULE. Suppose we have the RULE

    {-# RULE "map/coerce" map coerce = ... #-}

    As described in Note [Getting the map/coerce RULE to work],
    the occurrence of 'coerce' is transformed into:

    {-# RULE "map/coerce" forall (c :: T1 ~R# T2).
                          map ((\v -> v) `cast` c) = ... #-}

    We'd like 'map Age' to match the LHS. For this to happen, Age
    must be unfolded, otherwise we'll be stuck. This is tested in T16208.

It also allows for the possibility of representation-polymorphic newtypes
with wrappers (with -XUnliftedNewtypes):

  newtype N (a :: TYPE r) = MkN a

With -XUnliftedNewtypes, this is allowed -- even though MkN is representation-
polymorphic. It's OK because MkN evaporates in the compiled code, becoming
just a cast. That is, it has a compulsory unfolding. As long as its
argument is not representation-polymorphic (which it can't be, according to
Note [Representation polymorphism invariants] in GHC.Core), and it's saturated,
no representation-polymorphic code ends up in the code generator.
The saturation condition is effectively checked in
GHC.Tc.Gen.App.hasFixedRuntimeRep_remainingValArgs.

However, if we make a *wrapper* for a newtype, we get into trouble.
In that case, we generate a forbidden representation-polymorphic
binding, and we must then ensure that it is always instantiated
at a representation-monomorphic type.

The solution is simple, though: just make the newtype wrappers
as ephemeral as the newtype workers. In other words, give the wrappers
compulsory unfoldings and no bindings. The compulsory unfolding is given
in wrap_unf in mkDataConRep, and the lack of a binding happens in
GHC.Iface.Tidy.getTyConImplicitBinds, where we say that a newtype has no
implicit bindings.

Note [Records and linear types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All the fields, in a record constructor, are linear, because there is no syntax
to specify the type of record field. There will be (see the proposal
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#records-and-projections
), but it isn't implemented yet.

Projections of records can't be linear:

  data Foo = MkFoo { a :: A, b :: B }

If we had

  a :: Foo %1 -> A

We could write

  bad :: A %1 -> B %1 -> A
  bad x y = a (MkFoo { a=x, b=y })

There is an exception: if `b` (more generally all the fields besides `a`) is
unrestricted, then is perfectly possible to have a linear projection. Such a
linear projection has as simple definition.

  data Bar = MkBar { c :: C, d % Many :: D }

  c :: Bar %1 -> C
  c MkBar{ c=x, d=_} = x

The `% Many` syntax, for records, does not exist yet. But there is one important
special case which already happens: when there is a single field (usually a
newtype).

  newtype Baz = MkBaz { unbaz :: E }

unbaz could be linear. And, in fact, it is linear in the proposal design.

However, this hasn't been implemented yet.

************************************************************************
*                                                                      *
\subsection{Dictionary selectors}
*                                                                      *
************************************************************************

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.

Dictionary selectors may get nested forall-types.  Thus:

        class Foo a where
          op :: forall b. Ord b => a -> b -> b

Then the top-level type for op is

        op :: forall a. Foo a =>
              forall b. Ord b =>
              a -> b -> b

Note [Type classes and linear types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constraints, in particular type classes, don't have attached linearity
information. Implicitly, they are all unrestricted. See the linear types proposal,
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst .

When translating to core `C => ...` is always translated to an unrestricted
arrow `C % Many -> ...`.

Therefore there is no loss of generality if we make all selectors unrestricted.

-}

mkDictSelId :: Name          -- Name of one of the *value* selectors
                             -- (dictionary superclass or method)
            -> Class -> Id
mkDictSelId name clas
  = mkGlobalId (ClassOpId clas terminating) name sel_ty info
  where
    tycon          = classTyCon clas
    sel_names      = map idName (classAllSelIds clas)
    new_tycon      = isNewTyCon tycon
    [data_con]     = tyConDataCons tycon
    tyvars         = dataConUserTyVarBinders data_con
    n_ty_args      = length tyvars
    arg_tys        = dataConRepArgTys data_con  -- Includes the dictionary superclasses
    val_index      = assoc "MkId.mkDictSelId" (sel_names `zip` [0..]) name

    pred_ty = mkClassPred clas (mkTyVarTys (binderVars tyvars))
    res_ty  = scaledThing (getNth arg_tys val_index)
    sel_ty  = mkInvisForAllTys tyvars $
              mkFunctionType ManyTy pred_ty res_ty
             -- See Note [Type classes and linear types]

    terminating = isTerminatingType res_ty || definitelyUnliftedType res_ty
                  -- If the field is unlifted, it can't be bottom
                  -- Ditto if it's a terminating type

    base_info = noCafIdInfo
                `setArityInfo`  1
                `setDmdSigInfo` strict_sig
                `setCprSigInfo` topCprSig

    info | new_tycon
         = base_info `setInlinePragInfo` alwaysInlinePragma
                     `setUnfoldingInfo`  mkInlineUnfoldingWithArity defaultSimpleOpts
                                           StableSystemSrc 1
                                           (mkDictSelRhs clas val_index)
                   -- See Note [Single-method classes] in GHC.Tc.TyCl.Instance
                   -- for why alwaysInlinePragma

         | otherwise
         = base_info `setRuleInfo` mkRuleInfo [rule]
                     `setInlinePragInfo` neverInlinePragma
                     `setUnfoldingInfo`  mkInlineUnfoldingWithArity defaultSimpleOpts
                                           StableSystemSrc 1
                                           (mkDictSelRhs clas val_index)
                   -- Add a magic BuiltinRule, but no unfolding
                   -- so that the rule is always available to fire.
                   -- See Note [ClassOp/DFun selection] in GHC.Tc.TyCl.Instance

    -- This is the built-in rule that goes
    --      op (dfT d1 d2) --->  opT d1 d2
    rule = BuiltinRule { ru_name = fsLit "Class op " `appendFS`
                                     occNameFS (getOccName name)
                       , ru_fn    = name
                       , ru_nargs = n_ty_args + 1
                       , ru_try   = dictSelRule val_index n_ty_args }

        -- The strictness signature is of the form U(AAAVAAAA) -> T
        -- where the V depends on which item we are selecting
        -- It's worth giving one, so that absence info etc is generated
        -- even if the selector isn't inlined

    strict_sig = mkClosedDmdSig [arg_dmd] topDiv
    arg_dmd | new_tycon = evalDmd
            | otherwise = C_1N :* mkProd Unboxed dict_field_dmds
            where
              -- The evalDmd below is just a placeholder and will be replaced in
              -- GHC.Types.Demand.dmdTransformDictSel
              dict_field_dmds = [ if name == sel_name then evalDmd else absDmd
                                | sel_name <- sel_names ]

mkDictSelRhs :: Class
             -> Int         -- 0-indexed selector among (superclasses ++ methods)
             -> CoreExpr
mkDictSelRhs clas val_index
  = mkLams tyvars (Lam dict_id rhs_body)
  where
    tycon          = classTyCon clas
    new_tycon      = isNewTyCon tycon
    [data_con]     = tyConDataCons tycon
    tyvars         = dataConUnivTyVars data_con
    arg_tys        = dataConRepArgTys data_con  -- Includes the dictionary superclasses

    the_arg_id     = getNth arg_ids val_index
    pred           = mkClassPred clas (mkTyVarTys tyvars)
    dict_id        = mkTemplateLocal 1 pred
    arg_ids        = mkTemplateLocalsNum 2 (map scaledThing arg_tys)

    rhs_body | new_tycon = unwrapNewTypeBody tycon (mkTyVarTys tyvars)
                                                   (Var dict_id)
             | otherwise = mkSingleAltCase (Var dict_id) dict_id (DataAlt data_con)
                                           arg_ids (varToCoreExpr the_arg_id)
                                -- varToCoreExpr needed for equality superclass selectors
                                --   sel a b d = case x of { MkC _ (g:a~b) _ -> CO g }

dictSelRule :: Int -> Arity -> RuleFun
-- Tries to persuade the argument to look like a constructor
-- application, using exprIsConApp_maybe, and then selects
-- from it
--       sel_i t1..tk (D t1..tk op1 ... opm) = opi
--
dictSelRule val_index n_ty_args _ id_unf _ args
  | (dict_arg : _) <- drop n_ty_args args
  , Just (_, floats, _, _, con_args) <- exprIsConApp_maybe id_unf dict_arg
  = Just (wrapFloats floats $ getNth con_args val_index)
  | otherwise
  = Nothing

{-
************************************************************************
*                                                                      *
        Data constructors
*                                                                      *
************************************************************************
-}

mkDataConWorkId :: Name -> DataCon -> Id
mkDataConWorkId wkr_name data_con
  | isNewTyCon tycon
  = mkGlobalId (DataConWrapId data_con) wkr_name wkr_ty nt_work_info
      -- See Note [Newtype workers]

  | otherwise
  = mkGlobalId (DataConWorkId data_con) wkr_name wkr_ty alg_wkr_info

  where
    tycon  = dataConTyCon data_con  -- The representation TyCon
    wkr_ty = dataConRepType data_con

    ----------- Workers for data types --------------
    alg_wkr_info = noCafIdInfo
                   `setArityInfo`          wkr_arity
                   `setInlinePragInfo`     wkr_inline_prag
                   `setUnfoldingInfo`      evaldUnfolding  -- Record that it's evaluated,
                                                           -- even if arity = 0
          -- No strictness: see Note [Data-con worker strictness] in GHC.Core.DataCon

    wkr_inline_prag = defaultInlinePragma { inl_rule = ConLike }
    wkr_arity = dataConRepArity data_con

    ----------- Workers for newtypes --------------
    univ_tvs = dataConUnivTyVars data_con
    ex_tcvs  = dataConExTyCoVars data_con
    arg_tys  = dataConRepArgTys  data_con  -- Should be same as dataConOrigArgTys
    nt_work_info = noCafIdInfo          -- The NoCaf-ness is set by noCafIdInfo
                  `setArityInfo` 1      -- Arity 1
                  `setInlinePragInfo`     dataConWrapperInlinePragma
                  `setUnfoldingInfo`      newtype_unf
    id_arg1      = mkScaledTemplateLocal 1 (head arg_tys)
    res_ty_args  = mkTyCoVarTys univ_tvs
    newtype_unf  = assertPpr (null ex_tcvs && isSingleton arg_tys)
                             (ppr data_con)
                              -- Note [Newtype datacons]
                   mkCompulsoryUnfolding $
                   mkLams univ_tvs $ Lam id_arg1 $
                   wrapNewTypeBody tycon res_ty_args (Var id_arg1)

{-
-------------------------------------------------
--         Data constructor representation
--
-- This is where we decide how to wrap/unwrap the
-- constructor fields
--
--------------------------------------------------
-}

type Unboxer = Var -> UniqSM ([Var], CoreExpr -> CoreExpr)
  -- Unbox: bind rep vars by decomposing src var

data Boxer = UnitBox | Boxer (Subst -> UniqSM ([Var], CoreExpr))
  -- Box:   build src arg using these rep vars

-- | Data Constructor Boxer
newtype DataConBoxer = DCB ([Type] -> [Var] -> UniqSM ([Var], [CoreBind]))
                       -- Bind these src-level vars, returning the
                       -- rep-level vars to bind in the pattern

vanillaDataConBoxer :: DataConBoxer
-- No transformation on arguments needed
vanillaDataConBoxer = DCB (\_tys args -> return (args, []))

{-
Note [Inline partially-applied constructor wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We allow the wrapper to inline when partially applied to avoid
boxing values unnecessarily. For example, consider

   data Foo a = Foo !Int a

   instance Traversable Foo where
     traverse f (Foo i a) = Foo i <$> f a

This desugars to

   traverse f foo = case foo of
        Foo i# a -> let i = I# i#
                    in map ($WFoo i) (f a)

If the wrapper `$WFoo` is not inlined, we get a fruitless reboxing of `i`.
But if we inline the wrapper, we get

   map (\a. case i of I# i# a -> Foo i# a) (f a)

and now case-of-known-constructor eliminates the redundant allocation.

-}

data DataConBangOpts
  = FixedBangOpts [HsImplBang]
    -- ^ Used for imported data constructors
    -- See Note [Bangs on imported data constructors]
  | SrcBangOpts !BangOpts

data BangOpts = BangOpts
  { bang_opt_strict_data   :: !Bool -- ^ Strict fields by default
  , bang_opt_unbox_disable :: !Bool -- ^ Disable automatic field unboxing (e.g. if we aren't optimising)
  , bang_opt_unbox_strict  :: !Bool -- ^ Unbox strict fields
  , bang_opt_unbox_small   :: !Bool -- ^ Unbox small strict fields
  }

mkDataConRep :: DataConBangOpts
             -> FamInstEnvs
             -> Name
             -> DataCon
             -> UniqSM DataConRep
mkDataConRep dc_bang_opts fam_envs wrap_name data_con
  | not wrapper_reqd
  = return NoDataConRep

  | otherwise
  = do { wrap_args <- mapM (newLocal (fsLit "conrep")) wrap_arg_tys
       ; wrap_body <- mk_rep_app (dropList stupid_theta wrap_args `zip` dropList eq_spec unboxers)
                                 initial_wrap_app
                        -- Drop the stupid theta arguments, as per
                        -- Note [Instantiating stupid theta] in GHC.Core.DataCon.

       ; let wrap_id = mkGlobalId (DataConWrapId data_con) wrap_name wrap_ty wrap_info
             wrap_info = noCafIdInfo
                         `setArityInfo`         wrap_arity
                             -- It's important to specify the arity, so that partial
                             -- applications are treated as values
                         `setInlinePragInfo`    wrap_prag
                         `setUnfoldingInfo`     wrap_unf
                         `setDmdSigInfo`        wrap_sig
                             -- We need to get the CAF info right here because GHC.Iface.Tidy
                             -- does not tidy the IdInfo of implicit bindings (like the wrapper)
                             -- so it not make sure that the CAF info is sane

             -- The signature is purely for passes like the Simplifier, not for
             -- DmdAnal itself; see Note [DmdAnal for DataCon wrappers].
             wrap_sig = mkClosedDmdSig wrap_arg_dmds topDiv

             wrap_arg_dmds =
               replicate (length theta) topDmd ++ map mk_dmd arg_ibangs
               -- Don't forget the dictionary arguments when building
               -- the strictness signature (#14290).

             mk_dmd str | isBanged str = evalDmd
                        | otherwise    = topDmd

             wrap_prag = dataConWrapperInlinePragma
                         `setInlinePragmaActivation` activateDuringFinal
                         -- See Note [Activation for data constructor wrappers]

             -- The wrapper will usually be inlined (see wrap_unf), so its
             -- strictness and CPR info is usually irrelevant. But this is
             -- not always the case; GHC may choose not to inline it. In
             -- particular, the wrapper constructor is not inlined inside
             -- an INLINE rhs or when it is not applied to any arguments.
             -- See Note [Inline partially-applied constructor wrappers]
             -- Passing Nothing here allows the wrapper to inline when
             -- unsaturated.
             wrap_unf | isNewTyCon tycon = mkCompulsoryUnfolding wrap_rhs
                        -- See Note [Compulsory newtype unfolding]
                      | otherwise        = mkDataConUnfolding wrap_rhs
             wrap_rhs = mkLams wrap_tvs $
                        mkLams wrap_args $
                        wrapFamInstBody tycon res_ty_args $
                        wrap_body

       ; return (DCR { dcr_wrap_id = wrap_id
                     , dcr_boxer   = mk_boxer boxers
                     , dcr_arg_tys = rep_tys
                     , dcr_stricts = rep_strs
                       -- For newtypes, dcr_bangs is always [HsLazy].
                       -- See Note [HsImplBangs for newtypes].
                     , dcr_bangs   = arg_ibangs }) }

  where
    (univ_tvs, ex_tvs, eq_spec, theta, orig_arg_tys, _orig_res_ty)
                 = dataConFullSig data_con
    stupid_theta = dataConStupidTheta data_con
    wrap_tvs     = dataConUserTyVars data_con
    res_ty_args  = dataConResRepTyArgs data_con

    tycon        = dataConTyCon data_con       -- The representation TyCon (not family)
    wrap_ty      = dataConWrapperType data_con
    ev_tys       = eqSpecPreds eq_spec ++ theta
    all_arg_tys  = map unrestricted ev_tys ++ orig_arg_tys
    ev_ibangs    = map (const HsLazy) ev_tys
    orig_bangs   = dataConSrcBangs data_con

    wrap_arg_tys = (map unrestricted $ stupid_theta ++ theta) ++ orig_arg_tys
    wrap_arity   = count isCoVar ex_tvs + length wrap_arg_tys
             -- The wrap_args are the arguments *other than* the eq_spec
             -- Because we are going to apply the eq_spec args manually in the
             -- wrapper

    new_tycon = isNewTyCon tycon
    arg_ibangs
      | new_tycon
      = map (const HsLazy) orig_arg_tys -- See Note [HsImplBangs for newtypes]
                                        -- orig_arg_tys should be a singleton, but
                                        -- if a user declared a wrong newtype we
                                        -- detect this later (see test T2334A)
      | otherwise
      = case dc_bang_opts of
          SrcBangOpts bang_opts -> zipWith (dataConSrcToImplBang bang_opts fam_envs)
                                    orig_arg_tys orig_bangs
          FixedBangOpts bangs   -> bangs

    (rep_tys_w_strs, wrappers)
      = unzip (zipWith dataConArgRep all_arg_tys (ev_ibangs ++ arg_ibangs))

    (unboxers, boxers) = unzip wrappers
    (rep_tys, rep_strs) = unzip (concat rep_tys_w_strs)

    -- This is True if the data constructor or class dictionary constructor
    -- needs a wrapper. This wrapper is injected into the program later in the
    -- CoreTidy pass. See Note [Injecting implicit bindings] in GHC.Iface.Tidy,
    -- along with the accompanying implementation in getTyConImplicitBinds.
    wrapper_reqd =
        (not new_tycon
                     -- (Most) newtypes have only a worker, with the exception
                     -- of some newtypes written with GADT syntax.
                     -- See dataConUserTyVarsNeedWrapper below.
         && (any isBanged (ev_ibangs ++ arg_ibangs)))
                     -- Some forcing/unboxing (includes eq_spec)
      || isFamInstTyCon tycon -- Cast result
      || (dataConUserTyVarsNeedWrapper data_con
                     -- If the data type was written with GADT syntax and
                     -- orders the type variables differently from what the
                     -- worker expects, it needs a data con wrapper to reorder
                     -- the type variables.
                     -- See Note [Data con wrappers and GADT syntax].
                     --
                     -- NB: All GADTs return true from this function, but there
                     -- is one exception that we must check below.
         && not (isTypeDataTyCon tycon))
                     -- An exception to this rule is `type data` declarations.
                     -- Their data constructors only live at the type level and
                     -- therefore do not need wrappers.
                     -- See Note [Type data declarations] in GHC.Rename.Module.
                     --
                     -- Note that the other checks in this definition will
                     -- return False for `type data` declarations, as:
                     --
                     -- - They cannot be newtypes
                     -- - They cannot have strict fields
                     -- - They cannot be data family instances
                     -- - They cannot have datatype contexts
      || not (null stupid_theta)
                     -- If the data constructor has a datatype context,
                     -- we need a wrapper in order to drop the stupid arguments.
                     -- See Note [Instantiating stupid theta] in GHC.Core.DataCon.

    initial_wrap_app = Var (dataConWorkId data_con)
                       `mkTyApps`  res_ty_args
                       `mkVarApps` ex_tvs
                       `mkCoApps`  map (mkReflCo Nominal . eqSpecType) eq_spec

    mk_boxer :: [Boxer] -> DataConBoxer
    mk_boxer boxers = DCB (\ ty_args src_vars ->
                      do { let (ex_vars, term_vars) = splitAtList ex_tvs src_vars
                               subst1 = zipTvSubst univ_tvs ty_args
                               subst2 = extendTCvSubstList subst1 ex_tvs
                                                           (mkTyCoVarTys ex_vars)
                         ; (rep_ids, binds) <- go subst2 boxers term_vars
                         ; return (ex_vars ++ rep_ids, binds) } )

    go _ [] src_vars = assertPpr (null src_vars) (ppr data_con) $ return ([], [])
    go subst (UnitBox : boxers) (src_var : src_vars)
      = do { (rep_ids2, binds) <- go subst boxers src_vars
           ; return (src_var : rep_ids2, binds) }
    go subst (Boxer boxer : boxers) (src_var : src_vars)
      = do { (rep_ids1, arg)  <- boxer subst
           ; (rep_ids2, binds) <- go subst boxers src_vars
           ; return (rep_ids1 ++ rep_ids2, NonRec src_var arg : binds) }
    go _ (_:_) [] = pprPanic "mk_boxer" (ppr data_con)

    mk_rep_app :: [(Id,Unboxer)] -> CoreExpr -> UniqSM CoreExpr
    mk_rep_app [] con_app
      = return con_app
    mk_rep_app ((wrap_arg, unboxer) : prs) con_app
      = do { (rep_ids, unbox_fn) <- unboxer wrap_arg
           ; expr <- mk_rep_app prs (mkVarApps con_app rep_ids)
           ; return (unbox_fn expr) }


dataConWrapperInlinePragma :: InlinePragma
-- See Note [DataCon wrappers are conlike]
dataConWrapperInlinePragma =  alwaysInlineConLikePragma

{- Note [Activation for data constructor wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Activation on a data constructor wrapper allows it to inline only in FinalPhase.
This way rules have a chance to fire if they mention a data constructor on
the left
   RULE "foo"  f (K a b) = ...
Since the LHS of rules are simplified with InitialPhase, we won't
inline the wrapper on the LHS either.

On the other hand, this means that exprIsConApp_maybe must be able to deal
with wrappers so that case-of-constructor is not delayed; see
Note [exprIsConApp_maybe on data constructors with wrappers] for details.

It used to activate in phases 2 (afterInitial) and later, but it makes it
awkward to write a RULE[1] with a constructor on the left: it would work if a
constructor has no wrapper, but whether a constructor has a wrapper depends, for
instance, on the order of type argument of that constructors. Therefore changing
the order of type argument could make previously working RULEs fail.

See also https://gitlab.haskell.org/ghc/ghc/issues/15840 .

Note [DataCon wrappers are conlike]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DataCon workers are clearly ConLike --- they are the “Con” in
“ConLike”, after all --- but what about DataCon wrappers? Should they
be marked ConLike, too?

Yes, absolutely! As described in Note [CONLIKE pragma] in
GHC.Types.Basic, isConLike influences GHC.Core.Utils.exprIsExpandable,
which is used by both RULE matching and the case-of-known-constructor
optimization. It’s crucial that both of those things can see
applications of DataCon wrappers:

  * User-defined RULEs match on wrappers, not workers, so we might
    need to look through an unfolding built from a DataCon wrapper to
    determine if a RULE matches.

  * Likewise, if we have something like
        let x = $WC a b in ... case x of { C y z -> e } ...
    we still want to apply case-of-known-constructor.

Therefore, it’s important that we consider DataCon wrappers conlike.
This is especially true now that we don’t inline DataCon wrappers
until the final simplifier phase; see Note [Activation for data
constructor wrappers].

For further reading, see:
  * Note [Conlike is interesting] in GHC.Core.Op.Simplify.Utils
  * Note [Lone variables] in GHC.Core.Unfold
  * Note [exprIsConApp_maybe on data constructors with wrappers]
    in GHC.Core.SimpleOpt
  * #18012

Note [Bangs on imported data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We pass Maybe [HsImplBang] to mkDataConRep to make use of HsImplBangs
from imported modules.

- Nothing <=> use HsSrcBangs
- Just bangs <=> use HsImplBangs

For imported types we can't work it all out from the HsSrcBangs,
because we want to be very sure to follow what the original module
(where the data type was declared) decided, and that depends on what
flags were enabled when it was compiled. So we record the decisions in
the interface file.

The HsImplBangs passed are in 1-1 correspondence with the
dataConOrigArgTys of the DataCon.

Note [Data con wrappers and unlifted types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T = MkT !Int#

We certainly do not want to make a wrapper
   $WMkT x = case x of y { DEFAULT -> MkT y }

For a start, it's still to generate a no-op.  But worse, since wrappers
are currently injected at TidyCore, we don't even optimise it away!
So the stupid case expression stays there.  This actually happened for
the Integer data type (see #1600 comment:66)!

Note [Data con wrappers and GADT syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these two very similar data types:

  data T1 a b = MkT1 b

  data T2 a b where
    MkT2 :: forall b a. b -> T2 a b

Despite their similar appearance, T2 will have a data con wrapper but T1 will
not. What sets them apart? The types of their constructors, which are:

  MkT1 :: forall a b. b -> T1 a b
  MkT2 :: forall b a. b -> T2 a b

MkT2's use of GADT syntax allows it to permute the order in which `a` and `b`
would normally appear. See Note [DataCon user type variable binders] in GHC.Core.DataCon
for further discussion on this topic.

The worker data cons for T1 and T2, however, both have types such that `a` is
expected to come before `b` as arguments. Because MkT2 permutes this order, it
needs a data con wrapper to swizzle around the type variables to be in the
order the worker expects.

A somewhat surprising consequence of this is that *newtypes* can have data con
wrappers! After all, a newtype can also be written with GADT syntax:

  newtype T3 a b where
    MkT3 :: forall b a. b -> T3 a b

Again, this needs a wrapper data con to reorder the type variables. It does
mean that this newtype constructor requires another level of indirection when
being called, but the inliner should make swift work of that.

Note [HsImplBangs for newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most of the time, we use the dataConSrctoImplBang function to decide what
strictness/unpackedness to use for the fields of a data type constructor. But
there is an exception to this rule: newtype constructors. You might not think
that newtypes would pose a challenge, since newtypes are seemingly forbidden
from having strictness annotations in the first place. But consider this
(from #16141):

  {-# LANGUAGE StrictData #-}
  {-# OPTIONS_GHC -O #-}
  newtype T a b where
    MkT :: forall b a. Int -> T a b

Because StrictData (plus optimization) is enabled, invoking
dataConSrcToImplBang would sneak in and unpack the field of type Int to Int#!
This would be disastrous, since the wrapper for `MkT` uses a coercion involving
Int, not Int#.

Bottom line: dataConSrcToImplBang should never be invoked for newtypes. In the
case of a newtype constructor, we simply hardcode its dcr_bangs field to
[HsLazy].
-}

-------------------------

-- | Conjure a fresh local binder.
newLocal :: FastString   -- ^ a string which will form part of the 'Var'\'s name
         -> Scaled Type  -- ^ the type of the 'Var'
         -> UniqSM Var
newLocal name_stem (Scaled w ty) =
    mkSysLocalOrCoVarM name_stem w ty
         -- We should not have "OrCoVar" here, this is a bug (#17545)


-- | Unpack/Strictness decisions from source module.
--
-- This function should only ever be invoked for data constructor fields, and
-- never on the field of a newtype constructor.
-- See @Note [HsImplBangs for newtypes]@.
dataConSrcToImplBang
   :: BangOpts
   -> FamInstEnvs
   -> Scaled Type
   -> HsSrcBang
   -> HsImplBang

dataConSrcToImplBang bang_opts fam_envs arg_ty
                     (HsSrcBang ann unpk NoSrcStrict)
  | bang_opt_strict_data bang_opts -- StrictData => strict field
  = dataConSrcToImplBang bang_opts fam_envs arg_ty
                  (HsSrcBang ann unpk SrcStrict)
  | otherwise -- no StrictData => lazy field
  = HsLazy

dataConSrcToImplBang _ _ _ (HsSrcBang _ _ SrcLazy)
  = HsLazy

dataConSrcToImplBang bang_opts fam_envs arg_ty
                     (HsSrcBang _ unpk_prag SrcStrict)
  | isUnliftedType (scaledThing arg_ty)
    -- NB: non-newtype data constructors can't have representation-polymorphic fields
    -- so this is OK.
  = HsLazy  -- For !Int#, say, use HsLazy
            -- See Note [Data con wrappers and unlifted types]

  | let mb_co   = topNormaliseType_maybe fam_envs (scaledThing arg_ty)
                     -- Unwrap type families and newtypes
        arg_ty' = case mb_co of
                    { Just redn -> scaledSet arg_ty (reductionReducedType redn)
                    ; Nothing   -> arg_ty }
  , all (not . isNewTyCon . fst) (splitTyConApp_maybe $ scaledThing arg_ty')
  , shouldUnpackTy bang_opts unpk_prag fam_envs arg_ty'
  = if bang_opt_unbox_disable bang_opts
    then HsStrict True -- Not unpacking because of -O0
                       -- See Note [Detecting useless UNPACK pragmas] in GHC.Core.DataCon
    else case mb_co of
           Nothing   -> HsUnpack Nothing
           Just redn -> HsUnpack (Just $ reductionCoercion redn)

  | otherwise -- Record the strict-but-no-unpack decision
  = HsStrict False

-- | Wrappers/Workers and representation following Unpack/Strictness
-- decisions
dataConArgRep
  :: Scaled Type
  -> HsImplBang
  -> ([(Scaled Type,StrictnessMark)] -- Rep types
     ,(Unboxer,Boxer))

dataConArgRep arg_ty HsLazy
  = ([(arg_ty, NotMarkedStrict)], (unitUnboxer, unitBoxer))

dataConArgRep arg_ty (HsStrict _)
  = ([(arg_ty, MarkedStrict)], (seqUnboxer, unitBoxer))

dataConArgRep arg_ty (HsUnpack Nothing)
  = dataConArgUnpack arg_ty

dataConArgRep (Scaled w _) (HsUnpack (Just co))
  | let co_rep_ty = coercionRKind co
  , (rep_tys, wrappers) <- dataConArgUnpack (Scaled w co_rep_ty)
  = (rep_tys, wrapCo co co_rep_ty wrappers)


-------------------------
wrapCo :: Coercion -> Type -> (Unboxer, Boxer) -> (Unboxer, Boxer)
wrapCo co rep_ty (unbox_rep, box_rep)  -- co :: arg_ty ~ rep_ty
  = (unboxer, boxer)
  where
    unboxer arg_id = do { rep_id <- newLocal (fsLit "cowrap_unbx") (Scaled (idMult arg_id) rep_ty)
                        ; (rep_ids, rep_fn) <- unbox_rep rep_id
                        ; let co_bind = NonRec rep_id (Var arg_id `Cast` co)
                        ; return (rep_ids, Let co_bind . rep_fn) }
    boxer = Boxer $ \ subst ->
            do { (rep_ids, rep_expr)
                    <- case box_rep of
                         UnitBox -> do { rep_id <- newLocal (fsLit "cowrap_bx") (linear $ TcType.substTy subst rep_ty)
                                       ; return ([rep_id], Var rep_id) }
                         Boxer boxer -> boxer subst
               ; let sco = substCoUnchecked subst co
               ; return (rep_ids, rep_expr `Cast` mkSymCo sco) }

------------------------
seqUnboxer :: Unboxer
seqUnboxer v = return ([v], mkDefaultCase (Var v) v)

unitUnboxer :: Unboxer
unitUnboxer v = return ([v], \e -> e)

unitBoxer :: Boxer
unitBoxer = UnitBox

-------------------------

{- Note [UNPACK for sum types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a data type D, for example:
    data D = D1 [Int] [Bool]
           | D2

and another data type which unpacks a field of type D:
    data U a = MkU {-# UNPACK #-} !D
                   {-# UNPACK #-} !(a,a)
                   {-# UNPACK #-} !D

Then the wrapper and worker for MkU have these types

  -- Wrapper
  $WMkU :: D -> (a,a) -> D -> U a

  -- Worker
  MkU :: (# (# [Int],[Bool] #) | (# #) #)
      -> a
      -> a
      -> (# (# [Int],[Bool] #) | (# #) #)
      -> U a

For each unpacked /sum/-type argument, the worker gets one argument.
But for each unpacked /product/-type argument, the worker gets N
arguments (here two).

Why treat them differently?  See Note [Why sums and products are treated differently].

The wrapper $WMkU looks like this:

  $WMkU :: D -> (a,a) -> D -> U a
  $WMkU x1 y x2
    = case (case x1 of {
              D1 a b -> (# (# a,b #) | #)
              D2     -> (# | (# #) #) }) of { x1_ubx ->
      case y of { (y1, y2) ->
      case (case x2 of {
              D1 a b -> (# (# a,b #) | #)
              D2     -> (# | (# #) #) }) of { x2_ubx ->
      MkU x1_ubx y1 y2 x2_ubx

Notice the nested case needed for sums.

This different treatment for sums and product is implemented in
dataConArgUnpackSum and dataConArgUnpackProduct respectively.

Note [Why sums and products are treated differently]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Can we handle sums like products, with each wrapper argument
occupying multiple argument slots in the worker?  No: for a sum
type the number of argument slots varies, and that's exactly what
unboxed sums are designed for.

Can we handle products like sums, with each wrapper argument occupying
exactly one argument slot (and unboxed tuple) in the worker?  Yes,
we could.  For example
   data P = MkP {-# UNPACK #-} !Q
   data Q = MkQ {-# NOUNPACK #-} !Int
                {-# NOUNPACK #-} Int

Currently could unpack P thus, taking two slots in the worker
   $WMkP :: Q -> P
   $WMkP x = case x of { MkQ a b -> MkP a b }
   MkP :: Int -> Int -> P  -- Worker

We could instead do this (uniformly with sums)

   $WMkP1 :: Q -> P
   $WMkP1 x = case (case x of { MkQ a b -> (# a, b #) }) of ubx_x
              MkP1 ubx_x
   MkP1 :: (# Int, Int #) -> P  -- Worker

The representation of MkP and MkP1 would be identical (a constructor
with two fields).

BUT, with MkP (as with every data constructor) we record its argument
strictness as a bit-vector, actually [StrictnessMark]
   MkP strictness:  SL
This information is used in Core to record which fields are sure to
be evaluated.  (Look for calls to dataConRepStrictness.)  E.g. in Core
    case v of MkP x y -> ....<here x is known to be evald>....

Alas, with MkP1 this information is hidden by the unboxed pair,
In Core there will be an auxiliary case expression to take apart the pair:
    case v of MkP1 xy -> case xy of (# x,y #) -> ...
And now we have no easy way to know that x is evaluated in the "...".

Fixing this might be possible, but it'd be tricky.  So we avoid the
problem entirely by treating sums and products differently here.
-}

dataConArgUnpack
   :: Scaled Type
   ->  ( [(Scaled Type, StrictnessMark)]   -- Rep types
       , (Unboxer, Boxer) )
dataConArgUnpack scaledTy@(Scaled _ arg_ty)
  | Just (tc, tc_args) <- splitTyConApp_maybe arg_ty
  = assert (not (isNewTyCon tc)) $
    case tyConDataCons tc of
      [con] -> dataConArgUnpackProduct scaledTy tc_args con
      cons  -> dataConArgUnpackSum scaledTy tc_args cons
  | otherwise
  = pprPanic "dataConArgUnpack" (ppr arg_ty)
    -- An interface file specified Unpacked, but we couldn't unpack it

dataConArgUnpackProduct
  :: Scaled Type
  -> [Type]
  -> DataCon
  -> ( [(Scaled Type, StrictnessMark)]   -- Rep types
     , (Unboxer, Boxer) )
dataConArgUnpackProduct (Scaled arg_mult _) tc_args con =
  assert (null (dataConExTyCoVars con)) $
    -- Note [Unpacking GADTs and existentials]
  let rep_tys = map (scaleScaled arg_mult) $ dataConInstArgTys con tc_args
  in ( rep_tys `zip` dataConRepStrictness con
     , ( \ arg_id ->
         do { rep_ids <- mapM (newLocal (fsLit "unbx")) rep_tys
            ; let r_mult = idMult arg_id
            ; let rep_ids' = map (scaleIdBy r_mult) rep_ids
            ; let unbox_fn body
                    = mkSingleAltCase (Var arg_id) arg_id
                               (DataAlt con) rep_ids' body
            ; return (rep_ids, unbox_fn) }
       , Boxer $ \ subst ->
         do { rep_ids <- mapM (newLocal (fsLit "bx") . TcType.substScaledTyUnchecked subst) rep_tys
            ; return (rep_ids, Var (dataConWorkId con)
                               `mkTyApps` (substTysUnchecked subst tc_args)
                               `mkVarApps` rep_ids ) } ) )

dataConArgUnpackSum
  :: Scaled Type
  -> [Type]
  -> [DataCon]
  -> ( [(Scaled Type, StrictnessMark)]   -- Rep types
     , (Unboxer, Boxer) )
dataConArgUnpackSum (Scaled arg_mult arg_ty) tc_args cons =
  ( [ (sum_ty, MarkedStrict) ] -- The idea: Unpacked variant will
                               -- be one field only, and the type of the
                               -- field will be an unboxed sum.
  , ( unboxer, boxer ) )
  where
    !ubx_sum_arity = length cons
    src_tys = map (\con -> map scaledThing $ dataConInstArgTys con tc_args) cons
    sum_alt_tys = map mkUbxSumAltTy src_tys
    sum_ty_unscaled = mkSumTy sum_alt_tys
    sum_ty = Scaled arg_mult sum_ty_unscaled
    newLocal' fs = newLocal fs . Scaled arg_mult

    -- See Note [UNPACK for sum types]
    unboxer :: Unboxer
    unboxer arg_id = do
      con_arg_binders <- mapM (mapM (newLocal' (fsLit "unbx"))) src_tys
      ubx_sum_bndr <- newLocal (fsLit "unbx") sum_ty

      let
        mk_ubx_sum_alt :: Int -> DataCon -> [Var] -> CoreAlt
        mk_ubx_sum_alt alt con [bndr] = Alt (DataAlt con) [bndr]
            (mkCoreUnboxedSum ubx_sum_arity alt sum_alt_tys (Var bndr))

        mk_ubx_sum_alt alt con bndrs =
          let tuple = mkCoreUnboxedTuple (map Var bndrs)
           in Alt (DataAlt con) bndrs (mkCoreUnboxedSum ubx_sum_arity alt sum_alt_tys tuple )

        ubx_sum :: CoreExpr
        ubx_sum =
          let alts = zipWith3 mk_ubx_sum_alt [ 1 .. ] cons con_arg_binders
           in Case (Var arg_id) arg_id (coreAltsType alts) alts

        unbox_fn :: CoreExpr -> CoreExpr
        unbox_fn body =
          mkSingleAltCase ubx_sum ubx_sum_bndr DEFAULT [] body

      return ([ubx_sum_bndr], unbox_fn)

    boxer :: Boxer
    boxer = Boxer $ \ subst -> do
              unboxed_field_id <- newLocal' (fsLit "bx") (TcType.substTy subst sum_ty_unscaled)
              tuple_bndrs <- mapM (newLocal' (fsLit "bx") . TcType.substTy subst) sum_alt_tys

              let tc_args' = substTys subst tc_args
                  arg_ty' = substTy subst arg_ty

              con_arg_binders <-
                mapM (mapM (newLocal' (fsLit "bx")) . map (TcType.substTy subst)) src_tys

              let mk_sum_alt :: Int -> DataCon -> Var -> [Var] -> CoreAlt
                  mk_sum_alt alt con _ [datacon_bndr] =
                    ( Alt (DataAlt (sumDataCon alt ubx_sum_arity)) [datacon_bndr]
                      (Var (dataConWorkId con) `mkTyApps`  tc_args'
                                              `mkVarApps` [datacon_bndr] ))

                  mk_sum_alt alt con tuple_bndr datacon_bndrs =
                    ( Alt (DataAlt (sumDataCon alt ubx_sum_arity)) [tuple_bndr] (
                      Case (Var tuple_bndr) tuple_bndr arg_ty'
                        [ Alt (DataAlt (tupleDataCon Unboxed (length datacon_bndrs))) datacon_bndrs
                            (Var (dataConWorkId con) `mkTyApps`  tc_args'
                                                    `mkVarApps` datacon_bndrs ) ] ))

              return ( [unboxed_field_id],
                       Case (Var unboxed_field_id) unboxed_field_id arg_ty'
                            (zipWith4 mk_sum_alt [ 1 .. ] cons tuple_bndrs con_arg_binders) )

-- | Every alternative of an unboxed sum has exactly one field, and we use
-- unboxed tuples when we need more than one field. This generates an unboxed
-- tuple when necessary, to be used in unboxed sum alts.
mkUbxSumAltTy :: [Type] -> Type
mkUbxSumAltTy [ty] = ty
mkUbxSumAltTy tys  = mkTupleTy Unboxed tys

shouldUnpackTy :: BangOpts -> SrcUnpackedness -> FamInstEnvs -> Scaled Type -> Bool
-- True if we ought to unpack the UNPACK the argument type
-- See Note [Recursive unboxing]
-- We look "deeply" inside rather than relying on the DataCons
-- we encounter on the way, because otherwise we might well
-- end up relying on ourselves!
shouldUnpackTy bang_opts prag fam_envs ty
  | Just data_cons <- unpackable_type_datacons (scaledThing ty)
  , should_unpack data_cons
  , all ok_con data_cons
  = True
  | otherwise
  = False
  where
    ok_con :: DataCon -> Bool      -- True <=> OK to unpack
    ok_con top_con                 -- False <=> not safe
      = ok_args emptyNameSet top_con
       where
         top_con_name = getName top_con

         ok_args dcs con
           = all (ok_arg dcs) $
             (dataConOrigArgTys con `zip` dataConSrcBangs con)
             -- NB: dataConSrcBangs gives the *user* request;
             -- We'd get a black hole if we used dataConImplBangs

         ok_arg :: NameSet -> (Scaled Type, HsSrcBang) -> Bool
         ok_arg dcs (Scaled _ ty, bang)
           = not (attempt_unpack bang) || ok_ty dcs norm_ty
           where
             norm_ty = topNormaliseType fam_envs ty

         ok_ty :: NameSet -> Type -> Bool
         ok_ty dcs ty
           | Just data_cons <- unpackable_type_datacons ty
           = all (ok_rec_con dcs) data_cons
           | otherwise
           = True        -- NB True here, in contrast to False at top level

         -- For the "at the root" comments see
         -- Note [Recursive unboxing] wrinkle (W2)
         ok_rec_con dcs con
           | dc_name == top_con_name   = False  -- Recursion at the root
           | dc_name `elemNameSet` dcs = True   -- Not at the root
           | otherwise                 = ok_args (dcs `extendNameSet` dc_name) con
           where
             dc_name = getName con

    attempt_unpack :: HsSrcBang -> Bool
    attempt_unpack (HsSrcBang _ SrcUnpack NoSrcStrict)
      = bang_opt_strict_data bang_opts
    attempt_unpack (HsSrcBang _ SrcUnpack SrcStrict)
      = True
    attempt_unpack (HsSrcBang _  NoSrcUnpack SrcStrict)
      = True  -- Be conservative
    attempt_unpack (HsSrcBang _  NoSrcUnpack NoSrcStrict)
      = bang_opt_strict_data bang_opts -- Be conservative
    attempt_unpack _ = False

    -- Determine whether we ought to unpack a field,
    -- based on user annotations if present, and heuristics if not.
    should_unpack data_cons =
      case prag of
        SrcNoUnpack -> False -- {-# NOUNPACK #-}
        SrcUnpack   -> True  -- {-# UNPACK #-}
        NoSrcUnpack -- No explicit unpack pragma, so use heuristics
          | (_:_:_) <- data_cons
          -> False -- Don't unpack sum types automatically, but they can
                   -- be unpacked with an explicit source UNPACK.
          | otherwise
          -> bang_opt_unbox_strict bang_opts
             || (bang_opt_unbox_small bang_opts
                 && rep_tys `lengthAtMost` 1)  -- See Note [Unpack one-wide fields]
      where (rep_tys, _) = dataConArgUnpack ty


-- Given a type already assumed to have been normalized by topNormaliseType,
-- unpackable_type_datacons ty = Just datacons
-- iff ty is of the form
--     T ty1 .. tyn
-- and T is an algebraic data type (not newtype), in which no data
-- constructors have existentials, and datacons is the list of data
-- constructors of T.
unpackable_type_datacons :: Type -> Maybe [DataCon]
unpackable_type_datacons ty
  | Just (tc, _) <- splitTyConApp_maybe ty
  , not (isNewTyCon tc)
    -- Even though `ty` has been normalised, it could still
    -- be a /recursive/ newtype, so we must check for that
  , Just cons <- tyConDataCons_maybe tc
  , not (null cons)
  , all (null . dataConExTyCoVars) cons
  = Just cons -- See Note [Unpacking GADTs and existentials]
  | otherwise
  = Nothing

{-
Note [Unpacking GADTs and existentials]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is nothing stopping us unpacking a data type with equality
components, like
  data Equal a b where
    Equal :: Equal a a

And it'd be fine to unpack a product type with existential components
too, but that would require a bit more plumbing, so currently we don't.

So for now we require: null (dataConExTyCoVars data_con)
See #14978

Note [Unpack one-wide fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The flag UnboxSmallStrictFields ensures that any field that can
(safely) be unboxed to a word-sized unboxed field, should be so unboxed.
For example:

    data A = A Int#
    newtype B = B A
    data C = C !B
    data D = D !C
    data E = E !()
    data F = F !D
    data G = G !F !F

All of these should have an Int# as their representation, except
G which should have two Int#s.

However

    data T = T !(S Int)
    data S = S !a

Here we can represent T with an Int#.

Note [Recursive unboxing]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data R = MkR {-# UNPACK #-} !S Int
  data S = MkS {-# UNPACK #-} !Int
The representation arguments of MkR are the *representation* arguments
of S (plus Int); the rep args of MkS are Int#.  This is all fine.

But be careful not to try to unbox this!
        data T = MkT {-# UNPACK #-} !T Int
Because then we'd get an infinite number of arguments.

Note that it's the *argument* type that matters. This is fine:
        data S = MkS S !Int
because Int is non-recursive.

Wrinkles:

(W1) Here is a more complicated case:
        data S = MkS {-# UNPACK #-} !T Int
        data T = MkT {-# UNPACK #-} !S Int
     Each of S and T must decide independently whether to unpack
     and they had better not both say yes. So they must both say no.

(W2) As #23307 shows,  we /do/ want to unpack the second arg of the Yes
     data constructor in this example, despite the recursion in List:
       data List a = Nil | Cons a !(List a)
       data Unconsed a = Unconsed a !(List a)
       data MUnconsed a = No | Yes {-# UNPACK #-} !(Unconsed a)
     When looking at
       {-# UNPACK #-} (Unconsed a)
     we can take Unconsed apart, but then get into a loop with List.
     That's fine: we can still take Unconsed apart.  It's only if we
     have a loop /at the root/ that we must not unpack.

(W3) Also behave conservatively when there is no UNPACK pragma
        data T = MkS !T Int
     with -funbox-strict-fields or -funbox-small-strict-fields
     we need to behave as if there was an UNPACK pragma there.

************************************************************************
*                                                                      *
        Wrapping and unwrapping newtypes and type families
*                                                                      *
************************************************************************
-}

wrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
-- The wrapper for the data constructor for a newtype looks like this:
--      newtype T a = MkT (a,Int)
--      MkT :: forall a. (a,Int) -> T a
--      MkT = /\a. \(x:(a,Int)). x `cast` sym (CoT a)
-- where CoT is the coercion TyCon associated with the newtype
--
-- The call (wrapNewTypeBody T [a] e) returns the
-- body of the wrapper, namely
--      e `cast` (CoT [a])
--
-- If a coercion constructor is provided in the newtype, then we use
-- it, otherwise the wrap/unwrap are both no-ops

wrapNewTypeBody tycon args result_expr
  = assert (isNewTyCon tycon) $
    mkCast result_expr (mkSymCo co)
  where
    co = mkUnbranchedAxInstCo Representational (newTyConCo tycon) args []

-- When unwrapping, we do *not* apply any family coercion, because this will
-- be done via a CoPat by the type checker.  We have to do it this way as
-- computing the right type arguments for the coercion requires more than just
-- a splitting operation (cf, GHC.Tc.Gen.Pat.tcConPat).

unwrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
unwrapNewTypeBody tycon args result_expr
  = assert (isNewTyCon tycon) $
    mkCast result_expr (mkUnbranchedAxInstCo Representational (newTyConCo tycon) args [])

-- If the type constructor is a representation type of a data instance, wrap
-- the expression into a cast adjusting the expression type, which is an
-- instance of the representation type, to the corresponding instance of the
-- family instance type.
-- See Note [Wrappers for data instance tycons]
wrapFamInstBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
wrapFamInstBody tycon args body
  | Just co_con <- tyConFamilyCoercion_maybe tycon
  = mkCast body (mkSymCo (mkUnbranchedAxInstCo Representational co_con args []))
  | otherwise
  = body

{-
************************************************************************
*                                                                      *
* Foreign calls
*                                                                      *
************************************************************************
-}

-- For each ccall we manufacture a separate CCallOpId, giving it
-- a fresh unique, a type that is correct for this particular ccall,
-- and a CCall structure that gives the correct details about calling
-- convention etc.
--
-- The *name* of this Id is a local name whose OccName gives the full
-- details of the ccall, type and all.  This means that the interface
-- file reader can reconstruct a suitable Id

mkFCallId :: Unique -> ForeignCall -> Type -> Id
mkFCallId uniq fcall ty
  = assert (noFreeVarsOfType ty) $
    -- A CCallOpId should have no free type variables;
    -- when doing substitutions won't substitute over it
    mkGlobalId (FCallId fcall) name ty info
  where
    occ_str = renderWithContext defaultSDocContext (braces (ppr fcall <+> ppr ty))
    -- The "occurrence name" of a ccall is the full info about the
    -- ccall; it is encoded, but may have embedded spaces etc!

    name = mkFCallName uniq (mkFastString occ_str)

    info = noCafIdInfo
           `setArityInfo`  arity
           `setDmdSigInfo` strict_sig
           `setCprSigInfo` topCprSig

    (bndrs, _) = tcSplitPiTys ty
    arity      = count isAnonPiTyBinder bndrs
    strict_sig = mkVanillaDmdSig arity topDiv
    -- the call does not claim to be strict in its arguments, since they
    -- may be lifted (foreign import prim) and the called code doesn't
    -- necessarily force them. See #11076.
{-
************************************************************************
*                                                                      *
\subsection{DictFuns and default methods}
*                                                                      *
************************************************************************

Note [Dict funs and default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dict funs and default methods are *not* ImplicitIds.  Their definition
involves user-written code, so we can't figure out their strictness etc
based on fixed info, as we can for constructors and record selectors (say).

NB: See also Note [Exported LocalIds] in GHC.Types.Id
-}

mkDictFunId :: Name      -- Name to use for the dict fun;
            -> [TyVar]
            -> ThetaType
            -> Class
            -> [Type]
            -> Id
-- Implements the DFun Superclass Invariant (see GHC.Tc.TyCl.Instance)
-- See Note [Dict funs and default methods]

mkDictFunId dfun_name tvs theta clas tys
  = mkExportedLocalId (DFunId is_nt)
                      dfun_name
                      dfun_ty
  where
    is_nt = isNewTyCon (classTyCon clas)
    dfun_ty = TcType.tcMkDFunSigmaTy tvs theta (mkClassPred clas tys)

{-
************************************************************************
*                                                                      *
\subsection{Un-definable}
*                                                                      *
************************************************************************

These Ids can't be defined in Haskell.  They could be defined in
unfoldings in the wired-in GHC.Prim interface file, but we'd have to
ensure that they were definitely, definitely inlined, because there is
no curried identifier for them.  That's what mkCompulsoryUnfolding
does. Alternatively, we could add the definitions to mi_decls of ghcPrimIface
but it's not clear if this would be simpler.

coercionToken# is not listed in ghcPrimIds, since its type uses (~#)
which is not supposed to be used in expressions (GHC throws an assertion
failure when trying.)
-}

nullAddrName, seqName,
   realWorldName, voidPrimIdName, coercionTokenName,
   coerceName, proxyName,
   leftSectionName, rightSectionName :: Name
nullAddrName      = mkWiredInIdName gHC_PRIM  (fsLit "nullAddr#")      nullAddrIdKey      nullAddrId
seqName           = mkWiredInIdName gHC_PRIM  (fsLit "seq")            seqIdKey           seqId
realWorldName     = mkWiredInIdName gHC_PRIM  (fsLit "realWorld#")     realWorldPrimIdKey realWorldPrimId
voidPrimIdName    = mkWiredInIdName gHC_PRIM  (fsLit "void#")          voidPrimIdKey      voidPrimId
coercionTokenName = mkWiredInIdName gHC_PRIM  (fsLit "coercionToken#") coercionTokenIdKey coercionTokenId
coerceName        = mkWiredInIdName gHC_PRIM  (fsLit "coerce")         coerceKey          coerceId
proxyName         = mkWiredInIdName gHC_PRIM  (fsLit "proxy#")         proxyHashKey       proxyHashId
leftSectionName   = mkWiredInIdName gHC_PRIM  (fsLit "leftSection")    leftSectionKey     leftSectionId
rightSectionName  = mkWiredInIdName gHC_PRIM  (fsLit "rightSection")   rightSectionKey    rightSectionId

-- Names listed in magicIds; see Note [magicIds]
lazyIdName, oneShotName, nospecIdName :: Name
lazyIdName        = mkWiredInIdName gHC_MAGIC (fsLit "lazy")           lazyIdKey          lazyId
oneShotName       = mkWiredInIdName gHC_MAGIC (fsLit "oneShot")        oneShotKey         oneShotId
nospecIdName      = mkWiredInIdName gHC_MAGIC (fsLit "nospec")         nospecIdKey        nospecId

------------------------------------------------
proxyHashId :: Id
proxyHashId
  = pcMiscPrelId proxyName ty
       (noCafIdInfo `setUnfoldingInfo` evaldUnfolding) -- Note [evaldUnfoldings]
  where
    -- proxy# :: forall {k} (a:k). Proxy# k a
    --
    -- The visibility of the `k` binder is Inferred to match the type of the
    -- Proxy data constructor (#16293).
    [kv,tv] = mkTemplateKiTyVar liftedTypeKind (\x -> [x])
    kv_ty   = mkTyVarTy kv
    tv_ty   = mkTyVarTy tv
    ty      = mkInfForAllTy kv $ mkSpecForAllTy tv $ mkProxyPrimTy kv_ty tv_ty

------------------------------------------------
nullAddrId :: Id
-- nullAddr# :: Addr#
-- The reason it is here is because we don't provide
-- a way to write this literal in Haskell.
nullAddrId = pcMiscPrelId nullAddrName addrPrimTy info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding (Lit nullAddrLit)

------------------------------------------------
seqId :: Id     -- See Note [seqId magic]
seqId = pcMiscPrelId seqName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` inline_prag
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      arity

    inline_prag
         = alwaysInlinePragma `setInlinePragmaActivation` ActiveAfter
                 NoSourceText 0
                  -- Make 'seq' not inline-always, so that simpleOptExpr
                  -- (see GHC.Core.Subst.simple_app) won't inline 'seq' on the
                  -- LHS of rules.  That way we can have rules for 'seq';
                  -- see Note [seqId magic]

    -- seq :: forall (r :: RuntimeRep) a (b :: TYPE r). a -> b -> b
    ty  =
      mkInfForAllTy runtimeRep2TyVar
      $ mkSpecForAllTys [alphaTyVar, openBetaTyVar]
      $ mkVisFunTyMany alphaTy (mkVisFunTyMany openBetaTy openBetaTy)

    [x,y] = mkTemplateLocals [alphaTy, openBetaTy]
    rhs = mkLams ([runtimeRep2TyVar, alphaTyVar, openBetaTyVar, x, y]) $
          Case (Var x) x openBetaTy [Alt DEFAULT [] (Var y)]

    arity = 2

------------------------------------------------
lazyId :: Id    -- See Note [lazyId magic]
lazyId = pcMiscPrelId lazyIdName ty info
  where
    info = noCafIdInfo
    ty  = mkSpecForAllTys [alphaTyVar] (mkVisFunTyMany alphaTy alphaTy)

------------------------------------------------
noinlineIdName, noinlineConstraintIdName :: Name
noinlineIdName           = mkWiredInIdName gHC_MAGIC (fsLit "noinline")
                                           noinlineIdKey noinlineId
noinlineConstraintIdName = mkWiredInIdName gHC_MAGIC (fsLit "noinlineConstraint")
                                           noinlineConstraintIdKey noinlineConstraintId

noinlineId :: Id -- See Note [noinlineId magic]
noinlineId = pcMiscPrelId noinlineIdName ty info
  where
    info = noCafIdInfo
    ty  = mkSpecForAllTys [alphaTyVar] $
          mkVisFunTyMany alphaTy alphaTy

noinlineConstraintId :: Id -- See Note [noinlineId magic]
noinlineConstraintId = pcMiscPrelId noinlineConstraintIdName ty info
  where
    info = noCafIdInfo
    ty   = mkSpecForAllTys [alphaConstraintTyVar] $
           mkFunTy visArgConstraintLike ManyTy alphaTy alphaConstraintTy

------------------------------------------------
nospecId :: Id -- See Note [nospecId magic]
nospecId = pcMiscPrelId nospecIdName ty info
  where
    info = noCafIdInfo
    ty  = mkSpecForAllTys [alphaTyVar] (mkVisFunTyMany alphaTy alphaTy)

oneShotId :: Id -- See Note [The oneShot function]
oneShotId = pcMiscPrelId oneShotName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      arity
    ty  = mkInfForAllTys  [ runtimeRep1TyVar, runtimeRep2TyVar ] $
          mkSpecForAllTys [ openAlphaTyVar, openBetaTyVar ]      $
          mkVisFunTyMany fun_ty fun_ty
    fun_ty = mkVisFunTyMany openAlphaTy openBetaTy
    [body, x] = mkTemplateLocals [fun_ty, openAlphaTy]
    x' = setOneShotLambda x  -- Here is the magic bit!
    rhs = mkLams [ runtimeRep1TyVar, runtimeRep2TyVar
                 , openAlphaTyVar, openBetaTyVar
                 , body, x'] $
          Var body `App` Var x'
    arity = 2

----------------------------------------------------------------------
{- Note [Wired-in Ids for rebindable syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The functions leftSectionId, rightSectionId are
wired in here ONLY because they are used in a representation-polymorphic way
by the rebindable syntax mechanism. See GHC.Rename.Expr
Note [Handling overloaded and rebindable constructs].

Alas, we can't currently give Haskell definitions for
representation-polymorphic functions.

They have Compulsory unfoldings, so that the representation polymorphism
does not linger for long.
-}

-- See Note [Left and right sections] in GHC.Rename.Expr
-- See Note [Wired-in Ids for rebindable syntax]
--   leftSection :: forall r1 r2 n (a::TYPE r1) (b::TYPE r2).
--                  (a %n-> b) -> a %n-> b
--   leftSection f x = f x
-- Important that it is eta-expanded, so that (leftSection undefined `seq` ())
--   is () and not undefined
-- Important that is is multiplicity-polymorphic (test linear/should_compile/OldList)
leftSectionId :: Id
leftSectionId = pcMiscPrelId leftSectionName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      arity
    ty  = mkInfForAllTys  [runtimeRep1TyVar,runtimeRep2TyVar, multiplicityTyVar1] $
          mkSpecForAllTys [openAlphaTyVar,  openBetaTyVar]    $
          exprType body
    [f,x] = mkTemplateLocals [mkVisFunTy mult openAlphaTy openBetaTy, openAlphaTy]

    mult = mkTyVarTy multiplicityTyVar1 :: Mult
    xmult = setIdMult x mult

    rhs  = mkLams [ runtimeRep1TyVar, runtimeRep2TyVar, multiplicityTyVar1
                  , openAlphaTyVar,   openBetaTyVar   ] body
    body = mkLams [f,xmult] $ App (Var f) (Var xmult)
    arity = 2

-- See Note [Left and right sections] in GHC.Rename.Expr
-- See Note [Wired-in Ids for rebindable syntax]
--   rightSection :: forall r1 r2 r3 n1 n2 (a::TYPE r1) (b::TYPE r2) (c::TYPE r3).
--                   (a %n1 -> b %n2-> c) -> b %n2-> a %n1-> c
--   rightSection f y x = f x y
-- Again, multiplicity polymorphism is important
rightSectionId :: Id
rightSectionId = pcMiscPrelId rightSectionName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      arity
    ty  = mkInfForAllTys  [runtimeRep1TyVar,runtimeRep2TyVar,runtimeRep3TyVar
                          , multiplicityTyVar1, multiplicityTyVar2 ] $
          mkSpecForAllTys [openAlphaTyVar,  openBetaTyVar,   openGammaTyVar ]  $
          exprType body
    mult1 = mkTyVarTy multiplicityTyVar1
    mult2 = mkTyVarTy multiplicityTyVar2

    [f,x,y] = mkTemplateLocals [ mkScaledFunTys [ Scaled mult1 openAlphaTy
                                                , Scaled mult2 openBetaTy ] openGammaTy
                               , openAlphaTy, openBetaTy ]
    xmult = setIdMult x mult1
    ymult = setIdMult y mult2
    rhs  = mkLams [ runtimeRep1TyVar, runtimeRep2TyVar, runtimeRep3TyVar
                  , multiplicityTyVar1, multiplicityTyVar2
                  , openAlphaTyVar,   openBetaTyVar,    openGammaTyVar ] body
    body = mkLams [f,ymult,xmult] $ mkVarApps (Var f) [xmult,ymult]
    arity = 3

--------------------------------------------------------------------------------

coerceId :: Id
coerceId = pcMiscPrelId coerceName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setArityInfo`      2
    eqRTy     = mkTyConApp coercibleTyCon  [ tYPE_r,         a, b ]
    eqRPrimTy = mkTyConApp eqReprPrimTyCon [ tYPE_r, tYPE_r, a, b ]
    ty        = mkInvisForAllTys [ Bndr rv InferredSpec
                                 , Bndr av SpecifiedSpec
                                 , Bndr bv SpecifiedSpec ] $
                mkInvisFunTy eqRTy $
                mkVisFunTyMany a b

    bndrs@[rv,av,bv] = mkTemplateKiTyVar runtimeRepTy
                        (\r -> [mkTYPEapp r, mkTYPEapp r])

    [r, a, b] = mkTyVarTys bndrs
    tYPE_r    = mkTYPEapp r

    [eqR,x,eq] = mkTemplateLocals [eqRTy, a, eqRPrimTy]
    rhs = mkLams (bndrs ++ [eqR, x]) $
          mkWildCase (Var eqR) (unrestricted eqRTy) b $
          [Alt (DataAlt coercibleDataCon) [eq] (Cast (Var x) (mkCoVarCo eq))]

{-
Note [seqId magic]
~~~~~~~~~~~~~~~~~~
'GHC.Prim.seq' is special in several ways.

a) Its fixity is set in GHC.Iface.Load.ghcPrimIface

b) It has quite a bit of desugaring magic.
   See GHC.HsToCore.Utils Note [Desugaring seq] (1) and (2) and (3)

c) There is some special rule handing: Note [User-defined RULES for seq]

Historical note:
    In GHC.Tc.Gen.Expr we used to need a special typing rule for 'seq', to handle calls
    whose second argument had an unboxed type, e.g.  x `seq` 3#

    However, with representation polymorphism we can now give seq the type
    seq :: forall (r :: RuntimeRep) a (b :: TYPE r). a -> b -> b
    which handles this case without special treatment in the typechecker.

Note [User-defined RULES for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Roman found situations where he had
      case (f n) of _ -> e
where he knew that f (which was strict in n) would terminate if n did.
Notice that the result of (f n) is discarded. So it makes sense to
transform to
      case n of _ -> e

Rather than attempt some general analysis to support this, I've added
enough support that you can do this using a rewrite rule:

  RULE "f/seq" forall n.  seq (f n) = seq n

You write that rule.  When GHC sees a case expression that discards
its result, it mentally transforms it to a call to 'seq' and looks for
a RULE.  (This is done in GHC.Core.Opt.Simplify.trySeqRules.)  As usual, the
correctness of the rule is up to you.

VERY IMPORTANT: to make this work, we give the RULE an arity of 1, not 2.
If we wrote
  RULE "f/seq" forall n e.  seq (f n) e = seq n e
with rule arity 2, then two bad things would happen:

  - The magical desugaring done in Note [seqId magic] item (b)
    for saturated application of 'seq' would turn the LHS into
    a case expression!

  - The code in GHC.Core.Opt.Simplify.rebuildCase would need to actually supply
    the value argument, which turns out to be awkward.

See also: Note [User-defined RULES for seq] in GHC.Core.Opt.Simplify.


Note [lazyId magic]
~~~~~~~~~~~~~~~~~~~
lazy :: forall a. a -> a

'lazy' is used to make sure that a sub-expression, and its free variables,
are truly used call-by-need, with no code motion.  Key examples:

* pseq:    pseq a b = a `seq` lazy b
  We want to make sure that the free vars of 'b' are not evaluated
  before 'a', even though the expression is plainly strict in 'b'.

* catch:   catch a b = catch# (lazy a) b
  Again, it's clear that 'a' will be evaluated strictly (and indeed
  applied to a state token) but we want to make sure that any exceptions
  arising from the evaluation of 'a' are caught by the catch (see
  #11555).

Implementing 'lazy' is a bit tricky:

* It must not have a strictness signature: by being a built-in Id,
  all the info about lazyId comes from here, not from GHC.Magic.hi.
  This is important, because the strictness analyser will spot it as
  strict!

* It must not have an unfolding: it gets "inlined" by a HACK in
  CorePrep. It's very important to do this inlining *after* unfoldings
  are exposed in the interface file.  Otherwise, the unfolding for
  (say) pseq in the interface file will not mention 'lazy', so if we
  inline 'pseq' we'll totally miss the very thing that 'lazy' was
  there for in the first place. See #3259 for a real world
  example.

* Suppose CorePrep sees (catch# (lazy e) b).  At all costs we must
  avoid using call by value here:
     case e of r -> catch# r b
  Avoiding that is the whole point of 'lazy'.  So in CorePrep (which
  generate the 'case' expression for a call-by-value call) we must
  spot the 'lazy' on the arg (in CorePrep.cpeApp), and build a 'let'
  instead.

* lazyId is defined in GHC.Base, so we don't *have* to inline it.  If it
  appears un-applied, we'll end up just calling it.

Note [noinlineId magic]
~~~~~~~~~~~~~~~~~~~~~~~
'noinline' is used to make sure that a function f is never inlined,
e.g., as in 'noinline f x'.  We won't inline f because we never inline
lone variables (see Note [Lone variables] in GHC.Core.Unfold

You might think that we could implement noinline like this:
   {-# NOINLINE #-}
   noinline :: forall a. a -> a
   noinline x = x

But actually we give 'noinline' a wired-in name for three distinct reasons:

1. We don't want to leave a (useless) call to noinline in the final program,
   to be executed at runtime. So we have a little bit of magic to
   optimize away 'noinline' after we are done running the simplifier.
   This is done in GHC.CoreToStg.Prep.cpeApp.

2. 'noinline' sometimes gets inserted automatically when we serialize an
   expression to the interface format, in GHC.CoreToIface.toIfaceVar.
   See Note [Inlining and hs-boot files] in GHC.CoreToIface

3. Given foo :: Eq a => [a] -> Bool, the expression
     noinline foo x xs
   where x::Int, will naturally desugar to
      noinline @Int (foo @Int dEqInt) x xs
   But now it's entirely possible that (foo @Int dEqInt) will inline foo,
   since 'foo' is no longer a lone variable -- see #18995

   Solution: in the desugarer, rewrite
      noinline (f x y)  ==>  noinline f x y
   This is done in GHC.HsToCore.Utils.mkCoreAppDs.
   This is only needed for noinlineId, not noInlineConstraintId (wrinkle
   (W1) below), because the latter never shows up in user code.

Wrinkles

(W1) Sometimes case (2) above needs to apply `noinline` to a type of kind
     Constraint; e.g.
                    noinline @(Eq Int) $dfEqInt
     We don't have type-or-kind polymorphism, so we simply have two `inline`
     Ids, namely `noinlineId` and `noinlineConstraintId`.

(W2) Note that noinline as currently implemented can hide some simplifications
     since it hides strictness from the demand analyser. Specifically, the
     demand analyser will treat 'noinline f x' as lazy in 'x', even if the
     demand signature of 'f' specifies that it is strict in its argument. We
     considered fixing this this by adding a special case to the demand
     analyser to address #16588. However, the special case seemed like a large
     and expensive hammer to address a rare case and consequently we rather
     opted to use a more minimal solution.

Note [nospecId magic]
~~~~~~~~~~~~~~~~~~~~~
The 'nospec' magic Id is used to ensure to make a value opaque to the typeclass
specialiser. In CorePrep, we inline 'nospec', turning (nospec e) into e.
Note that this happens *after* unfoldings are exposed in the interface file.
This is crucial: otherwise, we could import an unfolding in which
'nospec' has been inlined (= erased), and we would lose the benefit.

'nospec' is used:

* In the implementation of 'withDict': we insert 'nospec' so that the
  typeclass specialiser doesn't assume any two evidence terms of the
  same type are equal. See Note [withDict] in GHC.Tc.Instance.Class,
  and see test case T21575b for an example.

* To defeat the specialiser when we have incoherent instances.
  See Note [Coherence and specialisation: overview] in GHC.Core.InstEnv.

Note [The oneShot function]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the context of making left-folds fuse somewhat okish (see ticket #7994
and Note [Left folds via right fold]) it was determined that it would be useful
if library authors could explicitly tell the compiler that a certain lambda is
called at most once. The oneShot function allows that.

'oneShot' is representation-polymorphic, i.e. the type variables can refer
to unlifted types as well (#10744); e.g.
   oneShot (\x:Int# -> x +# 1#)

Like most magic functions it has a compulsory unfolding, so there is no need
for a real definition somewhere. We have one in GHC.Magic for the convenience
of putting the documentation there.

It uses `setOneShotLambda` on the lambda's binder. That is the whole magic:

A typical call looks like
     oneShot (\y. e)
after unfolding the definition `oneShot = \f \x[oneshot]. f x` we get
     (\f \x[oneshot]. f x) (\y. e)
 --> \x[oneshot]. ((\y.e) x)
 --> \x[oneshot] e[x/y]
which is what we want.

It is only effective if the one-shot info survives as long as possible; in
particular it must make it into the interface in unfoldings. See Note [Preserve
OneShotInfo] in GHC.Core.Tidy.

Also see https://gitlab.haskell.org/ghc/ghc/wikis/one-shot.


-------------------------------------------------------------
@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@Literal@).

voidArgId is a Local Id used simply as an argument in functions
where we just want an arg to avoid having a thunk of unlifted type.
E.g.
        x = \ void :: Void# -> (# p, q #)

This comes up in strictness analysis

Note [evaldUnfoldings]
~~~~~~~~~~~~~~~~~~~~~~
The evaldUnfolding makes it look that some primitive value is
evaluated, which in turn makes Simplify.interestingArg return True,
which in turn makes INLINE things applied to said value likely to be
inlined.
-}

realWorldPrimId :: Id   -- :: State# RealWorld
realWorldPrimId = pcMiscPrelId realWorldName id_ty
                     (noCafIdInfo `setUnfoldingInfo` evaldUnfolding    -- Note [evaldUnfoldings]
                                  `setOneShotInfo`   typeOneShot id_ty)
   where
     id_ty = realWorldStatePrimTy

voidPrimId :: Id     -- Global constant :: Void#
                     -- The type Void# is now the same as (# #) (ticket #18441),
                     -- this identifier just signifies the (# #) datacon
                     -- and is kept for backwards compatibility.
                     -- We cannot define it in normal Haskell, since it's
                     -- a top-level unlifted value.
voidPrimId  = pcMiscPrelId voidPrimIdName unboxedUnitTy
                (noCafIdInfo `setUnfoldingInfo` mkCompulsoryUnfolding unboxedUnitExpr)

unboxedUnitExpr :: CoreExpr
unboxedUnitExpr = Var (dataConWorkId unboxedUnitDataCon)

voidArgId :: Id       -- Local lambda-bound :: Void#
voidArgId = mkSysLocal (fsLit "void") voidArgIdKey ManyTy unboxedUnitTy

coercionTokenId :: Id         -- :: () ~# ()
coercionTokenId -- See Note [Coercion tokens] in "GHC.CoreToStg"
  = pcMiscPrelId coercionTokenName
                 (mkTyConApp eqPrimTyCon [liftedTypeKind, liftedTypeKind, unitTy, unitTy])
                 noCafIdInfo

pcMiscPrelId :: Name -> Type -> IdInfo -> Id
pcMiscPrelId name ty info
  = mkVanillaGlobalWithInfo name ty info
