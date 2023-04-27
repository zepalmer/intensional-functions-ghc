module T23307 where

data List a = Nil | Cons a !(List a)
data Unconsed a = Unconsed a !(List a)
data MUnconsed a = No | Yes {-# UNPACK #-} !(Unconsed a)
