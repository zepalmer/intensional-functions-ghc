{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Intensional.Runtime.Syntax
( IntensionalFunctionSyntacticInput
) where

import Data.Kind (Type)
import GHC.TypeLits

import Control.Intensional.Runtime.NonEmptyList

type family IntensionalFunctionSyntacticInput (lst :: [Type])
        :: NonEmptyList Type where
    IntensionalFunctionSyntacticInput ('[]) =
        TypeError ('Text "Intensional function input must be non-empty")
    IntensionalFunctionSyntacticInput ('[a]) =
        'NonEmptyListSingleton a
    IntensionalFunctionSyntacticInput (a ': as) =
        'NonEmptyListCons a (IntensionalFunctionSyntacticInput as)
