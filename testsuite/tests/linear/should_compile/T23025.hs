{-# LANGUAGE LinearTypes, BangPatterns #-}
module T23025 where

f :: a %1 -> a
f !x = x
