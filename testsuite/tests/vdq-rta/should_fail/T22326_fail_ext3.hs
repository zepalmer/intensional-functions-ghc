{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NoScopedTypeVariables #-}

module T22326_fail_ext3 where

-- Binding a type variable requires ScopedTypeVariables.
f :: forall x -> Show x => x -> String
f (type t) = show @t