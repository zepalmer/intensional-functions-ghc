{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExistentialQuantification #-}
module Boot where

import A

data Data = forall n. Class n => D n
