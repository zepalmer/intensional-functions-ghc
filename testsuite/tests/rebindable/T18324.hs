{-# LANGUAGE ImpredicativeTypes, DeriveAnyClass #-}
-- {-# LANGUAGE MonadComprehensions, RecursiveDo #-}
module Main where


type Id = forall a. a -> a

t :: IO Id
t = return id

p :: Id -> (Bool, Int)
p f = (f True, f 3)

foo1 = t >>= \x -> return (p x)

foo2 = do { x <- t ; return (p x) }


main = do x <- foo2
          putStrLn $ show x
          
