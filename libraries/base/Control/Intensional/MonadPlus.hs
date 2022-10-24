{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Intensional.MonadPlus (
  IntensionalMonadPlus(..),
  itsGuard,
) where

import Control.Intensional.Alternative
import Control.Intensional.Applicative
import Control.Intensional.Functor
import Control.Intensional.Monad
import Control.Intensional.Runtime

class (IntensionalAlternative m, IntensionalMonad m)
    => IntensionalMonadPlus m where
  type IntensionalMonadPlusZeroC m a :: Constraint
  type IntensionalMonadPlusPlusC m a :: Constraint
  itsMzero :: (IntensionalMonadPlusZeroC m a) => m a
  itsMplus :: (IntensionalMonadPlusPlusC m a, IntensionalFunctorCF m ~ c)
           => '[m a, m a] ->%%c m a

itsGuard :: forall m c.
            ( IntensionalMonadPlus m
            , IntensionalApplicativePureC m ()
            , IntensionalMonadPlusZeroC m ()
            , c ~ IntensionalFunctorCF m
            )
         => Bool ->%c m ()
itsGuard = \%c condition -> if condition then itsPure %@ () else itsMzero
