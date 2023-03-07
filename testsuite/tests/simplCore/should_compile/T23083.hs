{-# OPTIONS_GHC -O2 -fforce-recomp #-}

module T23083 where

g :: ((Integer -> Integer) -> Integer) -> (Integer -> Integer) -> Integer
g f h = f (h `seq` (h $))
