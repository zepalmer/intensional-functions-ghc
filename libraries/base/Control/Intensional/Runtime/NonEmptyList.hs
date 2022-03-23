{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Intensional.Runtime.NonEmptyList
( NonEmptyList(..)
, NonEmptyListConcat
, NonEmptyListConstrainAll
, NonEmptyListSuffix
) where

import Control.Intensional.Runtime.Constraints

data NonEmptyList a
  = NonEmptyListSingleton a
  | NonEmptyListCons a (NonEmptyList a)

type family NonEmptyListConcat (a :: NonEmptyList k) (b :: NonEmptyList k)
    :: NonEmptyList k where
  NonEmptyListConcat ('NonEmptyListSingleton t1) b =
    'NonEmptyListCons t1 b
  NonEmptyListConcat ('NonEmptyListCons t a') b =
    'NonEmptyListCons t (NonEmptyListConcat a' b)

type family NonEmptyListConstrainAll
        (cfn :: ConstraintFn) (a :: NonEmptyList Type) :: Constraint where
    NonEmptyListConstrainAll cfn ('NonEmptyListSingleton t) =
        cfn t
    NonEmptyListConstrainAll cfn ('NonEmptyListCons t b) =
        (cfn t, NonEmptyListConstrainAll cfn b)

type family NonEmptyListSuffix
                (pfx :: NonEmptyList k)
                (lst :: NonEmptyList k)
                :: NonEmptyList k where
    NonEmptyListSuffix
        ('NonEmptyListSingleton t) ('NonEmptyListCons t lst) =
      lst
    NonEmptyListSuffix
        ('NonEmptyListCons t pfx) ('NonEmptyListCons t lst) =
      NonEmptyListSuffix pfx lst
