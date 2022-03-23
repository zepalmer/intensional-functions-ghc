{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Intensional.Functor (
  IntensionalFunctor(..),
  (%<$>)
) where

import Control.Intensional.Runtime
import Control.Intensional.Runtime.NonEmptyList
import Control.Intensional.UtilityFunctions

{- ========== Intensional Functor ========== -}

-- | An intensional functor is the intensional analogue of the (implicitly
--   extensional) @Functor@ typeclass.
--
--   While the usual Functor laws include
--     fmap id === id
--   that's only true here in spirit.  That is, the *point* of an intensional
--   functor is to capture the series of compositions that went on in the
--   @ifmap@ calls that were applied.  So the actual law here is
--     forget (ifmap %@ id) === forget id
--
--   Instances should probably be polite and provide a "forgetting" function
--   that translates from the intensional datatype to the extensional
--   equivalent.  We can't do that generically because the intensionality is
--   part of the data definition.  For instance, if a datatype @IntensionalList@
--   served as a wrapper around normal lists with an @IntensionalFunctor@
--   instance, it would be polite to provide a function
--   @ilistForget :: IntensionalList a -> [a]@ or similar.
class IntensionalFunctor (f :: Type -> Type) where
  -- | The constraint function for this intensional functor.  This should
  --   produce the constraint to which instances of this functor are subject
  --   when given the functor type itself.
  type IntensionalFunctorCF f :: ConstraintFn
  -- | Given the type of the functor and of the input and output leaf types of
  --   @ifmap@, produces a constraint that must be satisfied for mapping to be
  --   possible.
  type IntensionalFunctorMapC f a b :: Constraint
  -- | Maps over an intensional functor using the provided mapping function.
  itsFmap :: forall c a b.
             ( c ~ IntensionalFunctorCF f
             , IntensionalFunctorMapC f a b
             )
          => '[a ->%c b, f a] ->%%c f b

-- NOTE: Hask definitely doesn't have such a thing as a "free functor" because
--       there's no base case to work from.  Do not try to write a free
--       intensional functor.  The attempt will make you sad like it did us.

instance IntensionalFunctor
          (IntensionalFunction c ('NonEmptyListSingleton a)) where
  type IntensionalFunctorCF
        (IntensionalFunction c ('NonEmptyListSingleton a)) =
    c
  type IntensionalFunctorMapC
        (IntensionalFunction c ('NonEmptyListSingleton a)) a' b =
    ( Typeable c, Typeable a, Typeable a', Typeable b
    , c (a ->%c a'), c (a' ->%c b)
    )
  itsFmap = \%%c f g ->
    itsCompose @c %@+ NonEmptyHListCons f (NonEmptyHListSingleton g)

infixl 4 %<$>
(%<$>) :: ( IntensionalFunctor f
          , c ~ IntensionalFunctorCF f
          , IntensionalFunctorMapC f a b
          ) => '[a ->%c b, f a] ->%%c f b
(%<$>) = itsFmap
