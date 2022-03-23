{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Intensional.Alternative (
  IntensionalAlternative(..),
  IntensionalMonadPlus(..),
) where

import Control.Intensional.Applicative
import Control.Intensional.Functor
import Control.Intensional.Monad
import Control.Intensional.Runtime

class (IntensionalApplicative f)
    => IntensionalAlternative f where
  type IntensionalAlternativeEmptyC f a :: Constraint
  type IntensionalAlternativeChoiceC f a :: Constraint
  itsEmpty :: (IntensionalAlternativeEmptyC f a) => f a
  (%<|>) :: (IntensionalAlternativeChoiceC f a, IntensionalFunctorCF f ~ c)
         => f a ->%c f a ->%c f a

{- ========== Intensional MonadPlus ========== -}

class (IntensionalAlternative m, IntensionalMonad m)
    => IntensionalMonadPlus m where
  type IntensionalMonadPlusZeroC m a :: Constraint
  type IntensionalMonadPlusPlusC m a :: Constraint
  itsMzero :: (IntensionalMonadPlusZeroC m a) => m a
  itsMplus :: (IntensionalMonadPlusPlusC m a, IntensionalFunctorCF m ~ c)
           => m a ->%c m a ->%c m a
