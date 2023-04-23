-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1998

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}

-- | Type equality and comparison
module GHC.Core.TyCo.Compare (

    -- * Type comparison
    eqType, eqTypeX, eqTypes, nonDetCmpType, nonDetCmpTypes, nonDetCmpTypeX,
    nonDetCmpTypesX, nonDetCmpTc,
    eqVarBndrs,

    pickyEqType, tcEqType, tcEqKind, tcEqTypeNoKindCheck, tcEqTypeVis,
    tcEqTyConApps,

   -- * Visiblity comparision
   eqForAllVis, cmpForAllVis

   ) where

import GHC.Prelude

import GHC.Core.Type( typeKind, coreView, tcSplitAppTyNoView_maybe, splitAppTyNoView_maybe )

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCon

import GHC.Types.Var
import GHC.Types.Unique
import GHC.Types.Var.Env

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import GHC.Base (reallyUnsafePtrEquality#)

import qualified Data.Semigroup as S

{- GHC.Core.TyCo.Compare overview
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module implements type equality and comparison

It uses a few functions from GHC.Core.Type, notably `typeKind`,
so it currently sits "on top of" GHC.Core.Type.
-}

{- *********************************************************************
*                                                                      *
            Type equality
*                                                                      *
********************************************************************* -}

{- Note [Computing equality on types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module implements type equality, notably `eqType`. This is
"definitional equality" or just "equality" for short.

There are several places within GHC that depend on the precise choice of
definitional equality used. If we change that definition, all these places
must be updated. This Note merely serves as a place for all these places
to refer to, so searching for references to this Note will find every place
that needs to be updated.

* See Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep.

* See Historical Note [Typechecker equality vs definitional equality]
  below

Note [Type comparisons using object pointer comparisons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Quite often we substitute the type from a definition site into
occurances without a change. This means for code like:
    \x -> (x,x,x)
The type of every `x` will often be represented by a single object
in the heap. We can take advantage of this by shortcutting the equality
check if two types are represented by the same pointer under the hood.
In some cases this reduces compiler allocations by ~2%.
-}


tcEqKind :: HasDebugCallStack => Kind -> Kind -> Bool
tcEqKind = tcEqType

tcEqType :: HasDebugCallStack => Type -> Type -> Bool
-- ^ tcEqType implements typechecker equality
-- It behaves just like eqType, but is implemented
-- differently (for now)
tcEqType ty1 ty2
  =  tcEqTypeNoSyns ki1 ki2
  && tcEqTypeNoSyns ty1 ty2
  where
    ki1 = typeKind ty1
    ki2 = typeKind ty2

-- | Just like 'tcEqType', but will return True for types of different kinds
-- as long as their non-coercion structure is identical.
tcEqTypeNoKindCheck :: Type -> Type -> Bool
tcEqTypeNoKindCheck ty1 ty2
  = tcEqTypeNoSyns ty1 ty2

-- | Check whether two TyConApps are the same; if the number of arguments
-- are different, just checks the common prefix of arguments.
tcEqTyConApps :: TyCon -> [Type] -> TyCon -> [Type] -> Bool
tcEqTyConApps tc1 args1 tc2 args2
  = tc1 == tc2 &&
    and (zipWith tcEqTypeNoKindCheck args1 args2)
    -- No kind check necessary: if both arguments are well typed, then
    -- any difference in the kinds of later arguments would show up
    -- as differences in earlier (dependent) arguments

{-
Note [Specialising tc_eq_type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type equality predicates in Type are hit pretty hard during typechecking.
Consequently we take pains to ensure that these paths are compiled to
efficient, minimally-allocating code.

To this end we place an INLINE on tc_eq_type, ensuring that it is inlined into
its publicly-visible interfaces (e.g. tcEqType). In addition to eliminating
some dynamic branches, this allows the simplifier to eliminate the closure
allocations that would otherwise be necessary to capture the two boolean "mode"
flags. This reduces allocations by a good fraction of a percent when compiling
Cabal.

See #19226.
-}

-- | Type equality comparing both visible and invisible arguments and expanding
-- type synonyms.
tcEqTypeNoSyns :: Type -> Type -> Bool
tcEqTypeNoSyns ta tb = tc_eq_type False False ta tb

-- | Like 'tcEqType', but returns True if the /visible/ part of the types
-- are equal, even if they are really unequal (in the invisible bits)
tcEqTypeVis :: Type -> Type -> Bool
tcEqTypeVis ty1 ty2 = tc_eq_type False True ty1 ty2

-- | Like 'pickyEqTypeVis', but returns a Bool for convenience
pickyEqType :: Type -> Type -> Bool
-- Check when two types _look_ the same, _including_ synonyms.
-- So (pickyEqType String [Char]) returns False
-- This ignores kinds and coercions, because this is used only for printing.
pickyEqType ty1 ty2 = tc_eq_type True False ty1 ty2

-- | Real worker for 'tcEqType'. No kind check!
tc_eq_type :: Bool          -- ^ True <=> do not expand type synonyms
           -> Bool          -- ^ True <=> compare visible args only
           -> Type -> Type
           -> Bool
-- Flags False, False is the usual setting for tc_eq_type
-- See Note [Computing equality on types] in Type
tc_eq_type keep_syns vis_only orig_ty1 orig_ty2
  = go orig_env orig_ty1 orig_ty2
  where
    go :: RnEnv2 -> Type -> Type -> Bool
    -- See Note [Comparing nullary type synonyms] in GHC.Core.Type.
    go _   (TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2
      = True

    go env t1 t2 | not keep_syns, Just t1' <- coreView t1 = go env t1' t2
    go env t1 t2 | not keep_syns, Just t2' <- coreView t2 = go env t1 t2'

    go env (TyVarTy tv1) (TyVarTy tv2)
      = rnOccL env tv1 == rnOccR env tv2

    go _   (LitTy lit1) (LitTy lit2)
      = lit1 == lit2

    go env (ForAllTy (Bndr tv1 vis1) ty1)
           (ForAllTy (Bndr tv2 vis2) ty2)
      =  vis1 `eqForAllVis` vis2
      && (vis_only || go env (varType tv1) (varType tv2))
      && go (rnBndr2 env tv1 tv2) ty1 ty2

    -- Make sure we handle all FunTy cases since falling through to the
    -- AppTy case means that tcSplitAppTyNoView_maybe may see an unzonked
    -- kind variable, which causes things to blow up.
    -- See Note [Equality on FunTys] in GHC.Core.TyCo.Rep: we must check
    -- kinds here
    go env (FunTy _ w1 arg1 res1) (FunTy _ w2 arg2 res2)
      = kinds_eq && go env arg1 arg2 && go env res1 res2 && go env w1 w2
      where
        kinds_eq | vis_only  = True
                 | otherwise = go env (typeKind arg1) (typeKind arg2) &&
                               go env (typeKind res1) (typeKind res2)

      -- See Note [Equality on AppTys] in GHC.Core.Type
    go env (AppTy s1 t1)        ty2
      | Just (s2, t2) <- tcSplitAppTyNoView_maybe ty2
      = go env s1 s2 && go env t1 t2
    go env ty1                  (AppTy s2 t2)
      | Just (s1, t1) <- tcSplitAppTyNoView_maybe ty1
      = go env s1 s2 && go env t1 t2

    go env (TyConApp tc1 ts1)   (TyConApp tc2 ts2)
      = tc1 == tc2 && gos env (tc_vis tc1) ts1 ts2

    go env (CastTy t1 _)   t2              = go env t1 t2
    go env t1              (CastTy t2 _)   = go env t1 t2
    go _   (CoercionTy {}) (CoercionTy {}) = True

    go _ _ _ = False

    gos _   _         []       []      = True
    gos env (ig:igs) (t1:ts1) (t2:ts2) = (ig || go env t1 t2)
                                      && gos env igs ts1 ts2
    gos _ _ _ _ = False

    tc_vis :: TyCon -> [Bool]  -- True for the fields we should ignore
    tc_vis tc | vis_only  = inviss ++ repeat False    -- Ignore invisibles
              | otherwise = repeat False              -- Ignore nothing
       -- The repeat False is necessary because tycons
       -- can legitimately be oversaturated
      where
        bndrs = tyConBinders tc
        inviss  = map isInvisibleTyConBinder bndrs

    orig_env = mkRnEnv2 $ mkInScopeSet $ tyCoVarsOfTypes [orig_ty1, orig_ty2]

{-# INLINE tc_eq_type #-} -- See Note [Specialising tc_eq_type].


-- | Do these denote the same level of visibility? 'Required'
-- arguments are visible, others are not. So this function
-- equates 'Specified' and 'Inferred'. Used for printing.
eqForAllVis :: ForAllTyFlag -> ForAllTyFlag -> Bool
-- See Note [ForAllTy and type equality]
eqForAllVis Required      Required      = True
eqForAllVis (Invisible _) (Invisible _) = True
eqForAllVis _             _             = False

-- | Do these denote the same level of visibility? 'Required'
-- arguments are visible, others are not. So this function
-- equates 'Specified' and 'Inferred'. Used for printing.
cmpForAllVis :: ForAllTyFlag -> ForAllTyFlag -> Ordering
-- See Note [ForAllTy and type equality]
cmpForAllVis Required      Required       = EQ
cmpForAllVis Required      (Invisible {}) = LT
cmpForAllVis (Invisible _) Required       = GT
cmpForAllVis (Invisible _) (Invisible _)  = EQ


{- Note [ForAllTy and type equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we compare (ForAllTy (Bndr tv1 vis1) ty1)
         and    (ForAllTy (Bndr tv2 vis2) ty2)
what should we do about `vis1` vs `vis2`.

First, we always compare with `eqForAllVis` and `cmpForAllVis`.
But what decision do we make?

Should GHC type-check the following program (adapted from #15740)?

  {-# LANGUAGE PolyKinds, ... #-}
  data D a
  type family F :: forall k. k -> Type
  type instance F = D

Due to the way F is declared, any instance of F must have a right-hand side
whose kind is equal to `forall k. k -> Type`. The kind of D is
`forall {k}. k -> Type`, which is very close, but technically uses distinct
Core:

  -----------------------------------------------------------
  | Source Haskell    | Core                                |
  -----------------------------------------------------------
  | forall  k.  <...> | ForAllTy (Bndr k Specified) (<...>) |
  | forall {k}. <...> | ForAllTy (Bndr k Inferred)  (<...>) |
  -----------------------------------------------------------

We could deem these kinds to be unequal, but that would imply rejecting
programs like the one above. Whether a kind variable binder ends up being
specified or inferred can be somewhat subtle, however, especially for kinds
that aren't explicitly written out in the source code (like in D above).

For now, we decide

    the specified/inferred status of an invisible type variable binder
    does not affect GHC's notion of equality.

That is, we have the following:

  --------------------------------------------------
  | Type 1            | Type 2            | Equal? |
  --------------------|-----------------------------
  | forall k. <...>   | forall k. <...>   | Yes    |
  |                   | forall {k}. <...> | Yes    |
  |                   | forall k -> <...> | No     |
  --------------------------------------------------
  | forall {k}. <...> | forall k. <...>   | Yes    |
  |                   | forall {k}. <...> | Yes    |
  |                   | forall k -> <...> | No     |
  --------------------------------------------------
  | forall k -> <...> | forall k. <...>   | No     |
  |                   | forall {k}. <...> | No     |
  |                   | forall k -> <...> | Yes    |
  --------------------------------------------------

REMOVE-ME:
IMPORTANT NOTE: if we want to change this decision, ForAllCo will need to carry
visiblity (by taking a ForAllTyBinder rathre than a TyCoVar), so that
coercionLKind/RKind build forall types that match (are equal to) the desired
ones.  Otherwise we get an infinite loop in the solver via canEqCanLHSHetero.
Examples: T16946, T15079.

Historical Note [Typechecker equality vs definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note describes some history, in case there are vestiges of this
history lying around in the code.

Summary: prior to summer 2022, GHC had have two notions of equality
over Core types.  But now there is only one: definitional equality,
or just equality for short.

The old setup was:

* Definitional equality, as implemented by GHC.Core.Type.eqType.
  See Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep.

* Typechecker equality, as implemented by tcEqType.
  GHC.Tc.Solver.Canonical.canEqNC also respects typechecker equality.

Typechecker equality implied definitional equality: if two types are equal
according to typechecker equality, then they are also equal according to
definitional equality. The converse is not always true, as typechecker equality
is more finer-grained than definitional equality in two places:

* Constraint vs Type.  Definitional equality equated Type and
  Constraint, but typechecker treats them as distinct types.

* Unlike definitional equality, which does not care about the ForAllTyFlag of a
  ForAllTy, typechecker equality treats Required type variable binders as
  distinct from Invisible type variable binders.
  See Note [ForAllTy and type equality]


************************************************************************
*                                                                      *
                Comparison for types
        (We don't use instances so that we know where it happens)
*                                                                      *
************************************************************************

Note [Equality on AppTys]
~~~~~~~~~~~~~~~~~~~~~~~~~
In our cast-ignoring equality, we want to say that the following two
are equal:

  (Maybe |> co) (Int |> co')   ~?       Maybe Int

But the left is an AppTy while the right is a TyConApp. The solution is
to use splitAppTyNoView_maybe to break up the TyConApp into its pieces and
then continue. Easy to do, but also easy to forget to do.

Note [Comparing nullary type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the task of testing equality between two 'Type's of the form

  TyConApp tc []

where @tc@ is a type synonym. A naive way to perform this comparison these
would first expand the synonym and then compare the resulting expansions.

However, this is obviously wasteful and the RHS of @tc@ may be large; it is
much better to rather compare the TyCons directly. Consequently, before
expanding type synonyms in type comparisons we first look for a nullary
TyConApp and simply compare the TyCons if we find one. Of course, if we find
that the TyCons are *not* equal then we still need to perform the expansion as
their RHSs may still be equal.

We perform this optimisation in a number of places:

 * GHC.Core.Types.eqType
 * GHC.Core.Types.nonDetCmpType
 * GHC.Core.Unify.unify_ty
 * TcCanonical.can_eq_nc'
 * TcUnify.uType

This optimisation is especially helpful for the ubiquitous GHC.Types.Type,
since GHC prefers to use the type synonym over @TYPE 'LiftedRep@ applications
whenever possible. See Note [Using synonyms to compress types] in
GHC.Core.Type for details.

-}

eqType :: Type -> Type -> Bool
-- ^ Type equality on source types. Does not look through @newtypes@,
-- 'PredType's or type families, but it does look through type synonyms.
-- This first checks that the kinds of the types are equal and then
-- checks whether the types are equal, ignoring casts and coercions.
-- (The kind check is a recursive call, but since all kinds have type
-- @Type@, there is no need to check the types of kinds.)
-- See also Note [Non-trivial definitional equality] in "GHC.Core.TyCo.Rep".
eqType t1 t2 = isEqual $ nonDetCmpType t1 t2
  -- It's OK to use nonDetCmpType here and eqType is deterministic,
  -- nonDetCmpType does equality deterministically

-- | Compare types with respect to a (presumably) non-empty 'RnEnv2'.
eqTypeX :: RnEnv2 -> Type -> Type -> Bool
eqTypeX env t1 t2 = isEqual $ nonDetCmpTypeX env t1 t2
  -- It's OK to use nonDetCmpType here and eqTypeX is deterministic,
  -- nonDetCmpTypeX does equality deterministically

-- | Type equality on lists of types, looking through type synonyms
-- but not newtypes.
eqTypes :: [Type] -> [Type] -> Bool
eqTypes tys1 tys2 = isEqual $ nonDetCmpTypes tys1 tys2
  -- It's OK to use nonDetCmpType here and eqTypes is deterministic,
  -- nonDetCmpTypes does equality deterministically

eqVarBndrs :: RnEnv2 -> [Var] -> [Var] -> Maybe RnEnv2
-- Check that the var lists are the same length
-- and have matching kinds; if so, extend the RnEnv2
-- Returns Nothing if they don't match
eqVarBndrs env [] []
 = Just env
eqVarBndrs env (tv1:tvs1) (tv2:tvs2)
 | eqTypeX env (varType tv1) (varType tv2)
 = eqVarBndrs (rnBndr2 env tv1 tv2) tvs1 tvs2
eqVarBndrs _ _ _= Nothing

-- Now here comes the real worker

{-
Note [nonDetCmpType nondeterminism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nonDetCmpType is implemented in terms of nonDetCmpTypeX. nonDetCmpTypeX
uses nonDetCmpTc which compares TyCons by their Unique value. Using Uniques for
ordering leads to nondeterminism. We hit the same problem in the TyVarTy case,
comparing type variables is nondeterministic, note the call to nonDetCmpVar in
nonDetCmpTypeX.
See Note [Unique Determinism] for more details.
-}

nonDetCmpType :: Type -> Type -> Ordering
nonDetCmpType !t1 !t2
  -- See Note [Type comparisons using object pointer comparisons]
  | 1# <- reallyUnsafePtrEquality# t1 t2
  = EQ
nonDetCmpType (TyConApp tc1 []) (TyConApp tc2 []) | tc1 == tc2
  = EQ
nonDetCmpType t1 t2
  -- we know k1 and k2 have the same kind, because they both have kind *.
  = nonDetCmpTypeX rn_env t1 t2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfTypes [t1, t2]))
{-# INLINE nonDetCmpType #-}

nonDetCmpTypes :: [Type] -> [Type] -> Ordering
nonDetCmpTypes ts1 ts2 = nonDetCmpTypesX rn_env ts1 ts2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfTypes (ts1 ++ ts2)))

-- | An ordering relation between two 'Type's (known below as @t1 :: k1@
-- and @t2 :: k2@)
data TypeOrdering = TLT  -- ^ @t1 < t2@
                  | TEQ  -- ^ @t1 ~ t2@ and there are no casts in either,
                         -- therefore we can conclude @k1 ~ k2@
                  | TEQX -- ^ @t1 ~ t2@ yet one of the types contains a cast so
                         -- they may differ in kind.
                  | TGT  -- ^ @t1 > t2@
                  deriving (Eq, Ord, Enum, Bounded)

nonDetCmpTypeX :: RnEnv2 -> Type -> Type -> Ordering  -- Main workhorse
    -- See Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep
    -- See Note [Computing equality on types]
nonDetCmpTypeX env orig_t1 orig_t2 =
    case go env orig_t1 orig_t2 of
      -- If there are casts then we also need to do a comparison of
      -- the kinds of the types being compared
      TEQX          -> toOrdering $ go env k1 k2
      ty_ordering   -> toOrdering ty_ordering
  where
    k1 = typeKind orig_t1
    k2 = typeKind orig_t2

    toOrdering :: TypeOrdering -> Ordering
    toOrdering TLT  = LT
    toOrdering TEQ  = EQ
    toOrdering TEQX = EQ
    toOrdering TGT  = GT

    liftOrdering :: Ordering -> TypeOrdering
    liftOrdering LT = TLT
    liftOrdering EQ = TEQ
    liftOrdering GT = TGT

    thenCmpTy :: TypeOrdering -> TypeOrdering -> TypeOrdering
    thenCmpTy TEQ  rel  = rel
    thenCmpTy TEQX rel  = hasCast rel
    thenCmpTy rel  _    = rel

    hasCast :: TypeOrdering -> TypeOrdering
    hasCast TEQ = TEQX
    hasCast rel = rel

    -- Returns both the resulting ordering relation between
    -- the two types and whether either contains a cast.
    go :: RnEnv2 -> Type -> Type -> TypeOrdering
    -- See Note [Comparing nullary type synonyms].
    go _   (TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2
      = TEQ
    go env t1 t2
      | Just t1' <- coreView t1 = go env t1' t2
      | Just t2' <- coreView t2 = go env t1 t2'

    go env (TyVarTy tv1)       (TyVarTy tv2)
      = liftOrdering $ rnOccL env tv1 `nonDetCmpVar` rnOccR env tv2
    go env (ForAllTy (Bndr tv1 vis1) t1) (ForAllTy (Bndr tv2 vis2) t2)
      = liftOrdering (vis1 `cmpForAllVis` vis2)
        `thenCmpTy` go env (varType tv1) (varType tv2)
        `thenCmpTy` go (rnBndr2 env tv1 tv2) t1 t2

        -- See Note [Equality on AppTys]
    go env (AppTy s1 t1) ty2
      | Just (s2, t2) <- splitAppTyNoView_maybe ty2
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env ty1 (AppTy s2 t2)
      | Just (s1, t1) <- splitAppTyNoView_maybe ty1
      = go env s1 s2 `thenCmpTy` go env t1 t2

    go env (FunTy _ w1 s1 t1) (FunTy _ w2 s2 t2)
        -- NB: nonDepCmpTypeX does the kind check requested by
        -- Note [Equality on FunTys] in GHC.Core.TyCo.Rep
      = liftOrdering (nonDetCmpTypeX env s1 s2 S.<> nonDetCmpTypeX env t1 t2)
          `thenCmpTy` go env w1 w2
        -- Comparing multiplicities last because the test is usually true

    go env (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      = liftOrdering (tc1 `nonDetCmpTc` tc2) `thenCmpTy` gos env tys1 tys2

    go _   (LitTy l1)          (LitTy l2)          = liftOrdering (nonDetCmpTyLit l1 l2)
    go env (CastTy t1 _)       t2                  = hasCast $ go env t1 t2
    go env t1                  (CastTy t2 _)       = hasCast $ go env t1 t2

    go _   (CoercionTy {})     (CoercionTy {})     = TEQ

        -- Deal with the rest: TyVarTy < CoercionTy < AppTy < LitTy < TyConApp < ForAllTy
    go _ ty1 ty2
      = liftOrdering $ (get_rank ty1) `compare` (get_rank ty2)
      where get_rank :: Type -> Int
            get_rank (CastTy {})
              = pprPanic "nonDetCmpTypeX.get_rank" (ppr [ty1,ty2])
            get_rank (TyVarTy {})    = 0
            get_rank (CoercionTy {}) = 1
            get_rank (AppTy {})      = 3
            get_rank (LitTy {})      = 4
            get_rank (TyConApp {})   = 5
            get_rank (FunTy {})      = 6
            get_rank (ForAllTy {})   = 7

    gos :: RnEnv2 -> [Type] -> [Type] -> TypeOrdering
    gos _   []         []         = TEQ
    gos _   []         _          = TLT
    gos _   _          []         = TGT
    gos env (ty1:tys1) (ty2:tys2) = go env ty1 ty2 `thenCmpTy` gos env tys1 tys2

-------------
nonDetCmpTypesX :: RnEnv2 -> [Type] -> [Type] -> Ordering
nonDetCmpTypesX _   []        []        = EQ
nonDetCmpTypesX env (t1:tys1) (t2:tys2) = nonDetCmpTypeX env t1 t2 S.<>
                                          nonDetCmpTypesX env tys1 tys2
nonDetCmpTypesX _   []        _         = LT
nonDetCmpTypesX _   _         []        = GT

-------------
-- | Compare two 'TyCon's.
-- See Note [nonDetCmpType nondeterminism]
nonDetCmpTc :: TyCon -> TyCon -> Ordering
nonDetCmpTc tc1 tc2
  = u1 `nonDetCmpUnique` u2
  where
    u1  = tyConUnique tc1
    u2  = tyConUnique tc2



