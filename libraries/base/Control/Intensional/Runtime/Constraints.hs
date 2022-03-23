{-# LANGUAGE FlexibleInstances #-}

module Control.Intensional.Runtime.Constraints
( Constraint
, ConstraintFn
, NullConstraintFn
, Type
) where

import GHC.Exts (Constraint)
import Data.Kind (Type)

type ConstraintFn = Type -> Constraint

-- A constant constraint function which produces the null constraint.  This is
-- defined as a typeclass to allow partial application.
class NullConstraintFn a where
instance NullConstraintFn a where
