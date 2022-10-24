{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Intensional.Alternative (
  IntensionalAlternative(..),
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
         => '[f a, f a] ->%%c f a
