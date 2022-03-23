{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Intensional.Runtime.NonEmptyHList
( NonEmptyHList(..)
, SplitNonEmptyHList(..)
) where

import Data.Kind (Type)

import Control.Intensional.Runtime.NonEmptyList

data NonEmptyHList (a :: NonEmptyList Type) where
    NonEmptyHListSingleton :: a
                           -> NonEmptyHList ('NonEmptyListSingleton a)
    NonEmptyHListCons :: a
                      -> NonEmptyHList b
                      -> NonEmptyHList ('NonEmptyListCons a b)

class SplitNonEmptyHList (x :: NonEmptyList Type) (y :: NonEmptyList Type) where
    splitNonEmptyHList :: NonEmptyHList (NonEmptyListConcat x y)
                       -> (NonEmptyHList x, NonEmptyHList y)

instance SplitNonEmptyHList ('NonEmptyListSingleton t) ys where
    splitNonEmptyHList (NonEmptyHListCons x ys) = (NonEmptyHListSingleton x, ys)

instance (SplitNonEmptyHList xs ys)
        => SplitNonEmptyHList ('NonEmptyListCons x xs) ys where
    splitNonEmptyHList (NonEmptyHListCons x xys) =
        let (xs', ys') = splitNonEmptyHList xys in
        (NonEmptyHListCons x xs', ys')
