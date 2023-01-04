{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Intensional.Applicative (
  IntensionalApplicative(..),
  itsLiftA2,
) where

import Control.Intensional.Functor
import Control.Intensional.Runtime
import Control.Intensional.UtilityFunctions (itsCurry1)

{- ========== Intensional Applicative ========== -}

class (IntensionalFunctor f, Typeable f)
    => IntensionalApplicative (f :: Type -> Type) where
  type IntensionalApplicativePureC f a :: Constraint
  type IntensionalApplicativeApC f a b :: Constraint
  itsPure :: ( c ~ IntensionalFunctorCF f
             , IntensionalApplicativePureC f a
             )
          => a ->%c (f a)
  (%<*>) :: ( c ~ IntensionalFunctorCF f
            , IntensionalApplicativeApC f a b
            )
         => '[f (a ->%c b), f a] ->%%c (f b)

{-
  TODO: consider the following signature for (%<*>)

  '[ m (IntensionalFunction c inputs output)
   , m arg
   ] ->%%c m (IntensionalFunctionSingleApplicationResult cfn inputs output arg)

  Note that the current signature assumes that it's applying the function to
  saturation.
-}

infixl 4 %<*>

itsLiftA2 :: forall c f x y z.
             ( IntensionalApplicative f
             , IntensionalFunctorCF f ~ c
             , Typeable c, Typeable f, Typeable x, Typeable y, Typeable z
             , c ('[x,y] ->%%c z), c x
             , IntensionalFunctorMapC f x (y ->%c z)
             , IntensionalApplicativeApC f y z
             )
          => '[ '[x,y] ->%%c z
              , f x
              , f y
              ] ->%%c f z
itsLiftA2 = \%%c fn x y ->
  (%<*>) %@+
    NonEmptyHListCons
      (itsFmap %@+
        NonEmptyHListCons (itsCurry1 %@ fn) (NonEmptyHListSingleton x))
      (NonEmptyHListSingleton y)
