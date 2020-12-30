{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Provide some functions with the same names and interfaces as removed
-- primops.
module GHC.Prim.Deprecated
  (
  -- narrowing ops
    narrow8Int#
  , narrow16Int#
  , narrow32Int#
  , narrow8Word#
  , narrow16Word#
  , narrow32Word#
  ) where

import GHC.Prim
import GHC.Types () -- Make implicit dependency known to build system

default () -- Double and Integer aren't available yet

narrow8Int#   :: Int# -> Int#
narrow8Int# i = int8ToInt# (intToInt8# i)

narrow16Int#  :: Int# -> Int#
narrow16Int# i = int16ToInt# (intToInt16# i)

narrow32Int#  :: Int# -> Int#
narrow32Int# i = int32ToInt# (intToInt32# i)

narrow8Word#  :: Word# -> Word#
narrow8Word# i = word8ToWord# (wordToWord8# i)

narrow16Word# :: Word# -> Word#
narrow16Word# i = word16ToWord# (wordToWord16# i)

narrow32Word# :: Word# -> Word#
narrow32Word# i = word32ToWord# (wordToWord32# i)
