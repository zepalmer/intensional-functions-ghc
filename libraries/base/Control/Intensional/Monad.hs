{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Intensional.Monad (
  IntensionalMonad(..),
  itsReturn,
  itsThen,
  itsAp,
) where

import Control.Intensional.Applicative
import Control.Intensional.Functor
import Control.Intensional.Runtime

{- ========== Intensional Monad ========== -}

class (IntensionalApplicative m)
    => IntensionalMonad (m :: Type -> Type) where
  type IntensionalMonadBindC m a b :: Constraint
  itsBind :: ( c ~ IntensionalFunctorCF m, IntensionalMonadBindC m a b )
          => '[m a, a ->%c m b] ->%%c m b

itsReturn :: ( IntensionalMonad m
             , c ~ IntensionalFunctorCF m
             , IntensionalApplicativePureC m a
             )
          => a ->%c m a
itsReturn = itsPure

itsThen :: forall c m a b.
           ( IntensionalMonad m
           , c ~ IntensionalFunctorCF m
           , IntensionalMonadBindC m a b
           , Typeable a, Typeable b
           , c (m b)   -- This constraint is required due to \%c _ -> b    :(
           )
        => '[m a, m b] ->%%c m b
itsThen = \%%c a b ->
  itsBind %@% (a, \%c _ -> b)

itsAp :: forall c m a b.
         ( Typeable a, Typeable b
         , c (m a)
         , IntensionalFunctorCF m ~ c
         , IntensionalFunctorMapC m a b
         , IntensionalMonad m
         , IntensionalMonadBindC m (a ->%c b) b
         )
      => '[m (a ->%c b), m a] ->%%c m b
itsAp = \%%c f x ->
  intensional c do
    f' <- f
    itsFmap %@% (f', x)

