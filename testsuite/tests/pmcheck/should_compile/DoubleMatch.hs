{-# LANGUAGE CApiFFI, CPP, DeriveDataTypeable, NondecreasingIndentation #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -fno-cse #-}
module DoubleMatch where

data Handler = Default
             | Handler1

doingThing :: Handler -> IO Int
doingThing handler = do
  v <- case handler of
         Default -> return 0
         _other_Handler -> do
           asdf <- return 1
           let action = case handler of
                 Handler1 -> 1
           return action
  return v

-- doingThing123 :: Handler -> IO Int
-- doingThing123 handler = (>>=) 
--                         (case handler of
--                           Default -> return 0
--                           _other_handler ->  do
--                             asdf <- return 1
--                             let action = case handler of
--                                   Handler1 -> 1
--                             return action)
--                         (\v -> return v)


-- doingThing123 :: Handler -> IO Int
-- doingThing123 handler = (>>=) 
--                         (case handler of
--                           Default -> return 0
--                           _other_handler ->
--                             (>>=)(return 1) (\asdf ->
--                             let action = case handler of
--                                            Handler1 -> 1
--                             in
--                             return action))
--                         (\v -> return v)
