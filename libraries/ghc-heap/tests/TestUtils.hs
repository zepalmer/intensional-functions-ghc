{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module TestUtils
  ( assertEqual,
    assertThat,
    assertStackInvariants,
    getDecodedStack,
    unbox,
  )
where

import Control.Monad.IO.Class
import Data.Array.Byte
import Data.Foldable
import Debug.Trace
import GHC.Exts
import GHC.Exts.Heap
import GHC.Exts.Heap.Closures
import GHC.Exts.Stack.Decode
import GHC.Records
import GHC.Stack (HasCallStack)
import GHC.Stack.CloneStack
import Unsafe.Coerce (unsafeCoerce)

getDecodedStack :: IO (StackSnapshot, [StackFrame])
getDecodedStack = do
  stack <- cloneMyStack
  stackClosure <- decodeStack stack

  pure (stack, ssc_stack stackClosure)

assertEqual :: (HasCallStack, Monad m, Show a, Eq a) => a -> a -> m ()
assertEqual a b
  | a /= b = error (show a ++ " /= " ++ show b)
  | otherwise = pure ()

assertThat :: (HasCallStack, Monad m) => String -> (a -> Bool) -> a -> m ()
assertThat s f a = if f a then pure () else error s

assertStackInvariants :: (HasCallStack, MonadIO m) => [StackFrame] -> m ()
assertStackInvariants decodedStack =
  assertThat
    "Last frame is stop frame"
    ( \case
        StopFrame info -> tipe info == STOP_FRAME
        _ -> False
    )
    (last decodedStack)

unbox :: Box -> IO Closure
unbox = getBoxedClosureData
