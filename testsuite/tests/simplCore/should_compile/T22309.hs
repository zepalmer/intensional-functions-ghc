{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module ShouldCompile where

import GHC.Int
import GHC.Exts

-- These should unbox into another constructor
data UA = Mk_A !Int
data UB = Mk_B !Int64
data UC = Mk_C !Int32
data UD = Mk_D !Int32 !Int32
data UE = Mk_E !(# Int# #)
data UF = Mk_F !(# Double #)

-- These should not be unpacked into another constructor.
data NU_A = NU_MkA (# Int, Int #)
data NU_B = NU_MkB !Int64 !Int64

-- The types we unbox into

-- These should unpack their fields.
data WU_A = MkW_A !UA
data WU_B = MkW_B !UB
data WU_C = MkW_C !UC
data WU_D = MkW_D !UD
data WU_E = MkW_E !UE
data WU_F = MkW_F !UF

-- These should not unpack their fields.
data WNU_A = MkW_NA !NU_A
data WNU_B = MkW_NB !NU_B

-- data WrapInt = MkWrapInt (# Int# #)
-- data WrapInts = MkWrapInts (# Int#,Int#,Int# #)

-- -- We should unbox WrapInt here
-- data WrapWrapInt = MkWrapWrapInt !WrapInt
-- -- We should not unbox WrapIntLarge as it's large
-- data MkWrapWrapInts = MkWrapWrapInts !WrapInts


-- -- W will contain the ints directly
-- data T = T !(# Int, Int, Int, Int #)
-- data W = W !T

-- -- W2 will unbox T2, since it assumes the unboxed tuple to be a single value and doesn't
-- -- account for (# #) already being unboxed.
-- data T2 = T2 !( Int, Int, Int)
-- data W2 = W2 !T2
