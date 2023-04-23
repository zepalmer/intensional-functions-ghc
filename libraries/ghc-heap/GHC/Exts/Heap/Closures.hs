{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module GHC.Exts.Heap.Closures (
    -- * Closures
      Closure
    , GenClosure(..)
    , PrimType(..)
    , WhatNext(..)
    , WhyBlocked(..)
    , TsoFlags(..)
    , RetFunType(..)
    , allClosures
    , closureSize

    -- * Stack
    , StgStackClosure(..)
    , StackFrame(..)

    -- * Boxes
    , Box(..)
    , areBoxesEqual
    , asBox
    ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Exts.Heap.Constants
#if defined(PROFILING)
import GHC.Exts.Heap.InfoTableProf
#else
import GHC.Exts.Heap.InfoTable

-- `ghc -M` currently doesn't properly account for ways when generating
-- dependencies (#15197). This import ensures correct build-ordering between
-- this module and GHC.Exts.Heap.InfoTableProf. It should be removed when #15197
-- is fixed.
import GHC.Exts.Heap.InfoTableProf ()
#endif

import GHC.Exts.Heap.ProfInfo.Types

import Data.Bits
import Data.Foldable (toList)
import Data.Int
import Data.Word
import GHC.Exts
import GHC.Generics
import Numeric

------------------------------------------------------------------------
-- Boxes

foreign import prim "aToWordzh" aToWord# :: Any -> Word#

foreign import prim "reallyUnsafePtrEqualityUpToTag"
    reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#

-- | An arbitrary Haskell value in a safe Box. The point is that even
-- unevaluated thunks can safely be moved around inside the Box, and when
-- required, e.g. in 'getBoxedClosureData', the function knows how far it has
-- to evaluate the argument.
data Box = Box Any

instance Show Box where
-- From libraries/base/GHC/Ptr.lhs
   showsPrec _ (Box a) rs =
    -- unsafePerformIO (print "↓" >> pClosure a) `seq`
    pad_out (showHex addr "") ++ (if tag>0 then "/" ++ show tag else "") ++ rs
     where
       ptr  = W# (aToWord# a)
       tag  = ptr .&. fromIntegral tAG_MASK -- ((1 `shiftL` TAG_BITS) -1)
       addr = ptr - tag
       pad_out ls = '0':'x':ls

-- |This takes an arbitrary value and puts it into a box.
-- Note that calls like
--
-- > asBox (head list)
--
-- will put the thunk \"head list\" into the box, /not/ the element at the head
-- of the list. For that, use careful case expressions:
--
-- > case list of x:_ -> asBox x
asBox :: a -> Box
asBox x = Box (unsafeCoerce# x)

-- | Boxes can be compared, but this is not pure, as different heap objects can,
-- after garbage collection, become the same object.
areBoxesEqual :: Box -> Box -> IO Bool
areBoxesEqual (Box a) (Box b) = case reallyUnsafePtrEqualityUpToTag# a b of
    0# -> pure False
    _  -> pure True


------------------------------------------------------------------------
-- Closures
type Closure = GenClosure Box

-- | This is the representation of a Haskell value on the heap. It reflects
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/storage/Closures.h>
--
-- The data type is parametrized by `b`: the type to store references in.
-- Usually this is a 'Box' with the type synonym 'Closure'.
--
-- All Heap objects have the same basic layout. A header containing a pointer to
-- the info table and a payload with various fields. The @info@ field below
-- always refers to the info table pointed to by the header. The remaining
-- fields are the payload.
--
-- See
-- <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects>
-- for more information.
data GenClosure b
  = -- | A data constructor
    ConstrClosure
        { info       :: !StgInfoTable
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        , pkg        :: !String         -- ^ Package name
        , modl       :: !String         -- ^ Module name
        , name       :: !String         -- ^ Constructor name
        }

    -- | A function
  | FunClosure
        { info       :: !StgInfoTable
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk, an expression not obviously in head normal form
  | ThunkClosure
        { info       :: !StgInfoTable
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk which performs a simple selection operation
  | SelectorClosure
        { info       :: !StgInfoTable
        , selectee   :: !b              -- ^ Pointer to the object being
                                        --   selected from
        }

    -- | An unsaturated function application
  | PAPClosure
        { info       :: !StgInfoTable
        , arity      :: !HalfWord       -- ^ Arity of the partial application
        , n_args     :: !HalfWord       -- ^ Size of the payload in words
        , fun        :: !b              -- ^ Pointer to a 'FunClosure'
        , payload    :: ![b]            -- ^ Sequence of already applied
                                        --   arguments
        }

    -- In GHCi, if Linker.h would allow a reverse lookup, we could for exported
    -- functions fun actually find the name here.
    -- At least the other direction works via "lookupSymbol
    -- base_GHCziBase_zpzp_closure" and yields the same address (up to tags)
    -- | A function application
  | APClosure
        { info       :: !StgInfoTable
        , arity      :: !HalfWord       -- ^ Always 0
        , n_args     :: !HalfWord       -- ^ Size of payload in words
        , fun        :: !b              -- ^ Pointer to a 'FunClosure'
        , payload    :: ![b]            -- ^ Sequence of already applied
                                        --   arguments
        }

    -- | A suspended thunk evaluation
  | APStackClosure
        { info       :: !StgInfoTable
        , fun        :: !b              -- ^ Function closure
        , payload    :: ![b]            -- ^ Stack right before suspension
        }

    -- | A pointer to another closure, introduced when a thunk is updated
    -- to point at its value
  | IndClosure
        { info       :: !StgInfoTable
        , indirectee :: !b              -- ^ Target closure
        }

   -- | A byte-code object (BCO) which can be interpreted by GHC's byte-code
   -- interpreter (e.g. as used by GHCi)
  | BCOClosure
        { info       :: !StgInfoTable
        , instrs     :: !b              -- ^ A pointer to an ArrWords
                                        --   of instructions
        , literals   :: !b              -- ^ A pointer to an ArrWords
                                        --   of literals
        , bcoptrs    :: !b              -- ^ A pointer to an ArrWords
                                        --   of byte code objects
        , arity      :: !HalfWord       -- ^ The arity of this BCO
        , size       :: !HalfWord       -- ^ The size of this BCO in words
        , bitmap     :: ![Word]         -- ^ An StgLargeBitmap describing the
                                        --   pointerhood of its args/free vars
        }

    -- | A thunk under evaluation by another thread
  | BlackholeClosure
        { info       :: !StgInfoTable
        , indirectee :: !b              -- ^ The target closure
        }

    -- | A @ByteArray#@
  | ArrWordsClosure
        { info       :: !StgInfoTable
        , bytes      :: !Word           -- ^ Size of array in bytes
        , arrWords   :: ![Word]         -- ^ Array payload
        }

    -- | A @MutableByteArray#@
  | MutArrClosure
        { info       :: !StgInfoTable
        , mccPtrs    :: !Word           -- ^ Number of pointers
        , mccSize    :: !Word           -- ^ ?? Closures.h vs ClosureMacros.h
        , mccPayload :: ![b]            -- ^ Array payload
        -- Card table ignored
        }

    -- | A @SmallMutableArray#@
    --
    -- @since 8.10.1
  | SmallMutArrClosure
        { info       :: !StgInfoTable
        , mccPtrs    :: !Word           -- ^ Number of pointers
        , mccPayload :: ![b]            -- ^ Array payload
        }

  -- | An @MVar#@, with a queue of thread state objects blocking on them
  | MVarClosure
    { info       :: !StgInfoTable
    , queueHead  :: !b              -- ^ Pointer to head of queue
    , queueTail  :: !b              -- ^ Pointer to tail of queue
    , value      :: !b              -- ^ Pointer to closure
    }

    -- | An @IOPort#@, with a queue of thread state objects blocking on them
  | IOPortClosure
        { info       :: !StgInfoTable
        , queueHead  :: !b              -- ^ Pointer to head of queue
        , queueTail  :: !b              -- ^ Pointer to tail of queue
        , value      :: !b              -- ^ Pointer to closure
        }

    -- | A @MutVar#@
  | MutVarClosure
        { info       :: !StgInfoTable
        , var        :: !b              -- ^ Pointer to contents
        }

    -- | An STM blocking queue.
  | BlockingQueueClosure
        { info       :: !StgInfoTable
        , link       :: !b              -- ^ ?? Here so it looks like an IND
        , blackHole  :: !b              -- ^ The blackhole closure
        , owner      :: !b              -- ^ The owning thread state object
        , queue      :: !b              -- ^ ??
        }

  | WeakClosure
        { info        :: !StgInfoTable
        , cfinalizers :: !b
        , key         :: !b
        , value       :: !b
        , finalizer   :: !b
        , weakLink    :: !(Maybe b) -- ^ next weak pointer for the capability
        }

  -- | Representation of StgTSO: A Thread State Object. The values for
  -- 'what_next', 'why_blocked' and 'flags' are defined in @Constants.h@.
  | TSOClosure
      { info                :: !StgInfoTable
      -- pointers
      , link                :: !b
      , global_link         :: !b
      , tsoStack            :: !b -- ^ stackobj from StgTSO
      , trec                :: !b
      , blocked_exceptions  :: !b
      , bq                  :: !b
      , thread_label        :: !(Maybe b)
      -- values
      , what_next           :: !WhatNext
      , why_blocked         :: !WhyBlocked
      , flags               :: ![TsoFlags]
      , threadId            :: !Word64
      , saved_errno         :: !Word32
      , tso_dirty           :: !Word32 -- ^ non-zero => dirty
      , alloc_limit         :: !Int64
      , tot_stack_size      :: !Word32
      , prof                :: !(Maybe StgTSOProfInfo)
      }

  -- | Representation of StgStack: The 'tsoStack ' of a 'TSOClosure'.
  | StackClosure
      { info            :: !StgInfoTable
      , stack_size      :: !Word32 -- ^ stack size in *words*
      , stack_dirty     :: !Word8 -- ^ non-zero => dirty
#if __GLASGOW_HASKELL__ >= 811
      , stack_marking   :: !Word8
#endif
      }

    ------------------------------------------------------------
    -- Unboxed unlifted closures

    -- | Primitive Int
  | IntClosure
        { ptipe      :: PrimType
        , intVal     :: !Int }

    -- | Primitive Word
  | WordClosure
        { ptipe      :: PrimType
        , wordVal    :: !Word }

    -- | Primitive Int64
  | Int64Closure
        { ptipe      :: PrimType
        , int64Val   :: !Int64 }

    -- | Primitive Word64
  | Word64Closure
        { ptipe      :: PrimType
        , word64Val  :: !Word64 }

    -- | Primitive Addr
  | AddrClosure
        { ptipe      :: PrimType
        , addrVal    :: !(Ptr ()) }

    -- | Primitive Float
  | FloatClosure
        { ptipe      :: PrimType
        , floatVal   :: !Float }

    -- | Primitive Double
  | DoubleClosure
        { ptipe      :: PrimType
        , doubleVal  :: !Double }

    -----------------------------------------------------------
    -- Anything else

    -- | Another kind of closure
  | OtherClosure
        { info       :: !StgInfoTable
        , hvalues    :: ![b]
        , rawWords   :: ![Word]
        }

  | UnsupportedClosure
        { info       :: !StgInfoTable
        }

    -- | A primitive word from a bitmap encoded stack frame payload
    --
    -- The type itself cannot be restored (i.e. it might also represent a byte
    -- or an int).
  |  UnknownTypeWordSizedPrimitive
        { wordVal :: !Word }
  deriving (Show, Generic, Functor, Foldable, Traversable)

-- | A decoded @StgStack@ with `StackFrame`s
--
-- This is separate from it's `Closure` incarnation, as unification would
-- require two kinds of boxes for bitmap encoded stack content: One for
-- primitives and one for closures. This turned out to be a nightmare with lots
-- of pattern matches and leaking data structures to enable access to primitives
-- on the stack...
data  StgStackClosure = StgStackClosure
      { ssc_info            :: !StgInfoTable
      , ssc_stack_size      :: !Word32 -- ^ stack size in *words*
      , ssc_stack_dirty     :: !Word8 -- ^ non-zero => dirty
      , ssc_stack_marking   :: !Word8
      , ssc_stack           :: ![StackFrame]
      }
      deriving Show

-- | A single stack frame
--
-- It doesn't use `Box`es because that would require a `Box` constructor for
-- primitive values (bitmap encoded payloads), which introduces lots of pattern
-- matches and complicates the whole implementation (and breaks existing code.)
data StackFrame =
   UpdateFrame
      { info_tbl           :: !StgInfoTable
      , updatee            :: !Closure
      }

  | CatchFrame
      { info_tbl            :: !StgInfoTable
      , exceptions_blocked  :: Word
      , handler             :: !Closure
      }

  | CatchStmFrame
      { info_tbl            :: !StgInfoTable
      , catchFrameCode      :: !Closure
      , handler             :: !Closure
      }

  | CatchRetryFrame
      { info_tbl            :: !StgInfoTable
      , running_alt_code    :: !Word
      , first_code          :: !Closure
      , alt_code            :: !Closure
      }

  | AtomicallyFrame
      { info_tbl            :: !StgInfoTable
      , atomicallyFrameCode :: !Closure
      , result              :: !Closure
      }

  | UnderflowFrame
      { info_tbl            :: !StgInfoTable
      , nextChunk           :: !StgStackClosure
      }

  | StopFrame
      { info_tbl            :: !StgInfoTable }

  | RetSmall
      { info_tbl            :: !StgInfoTable
      , stack_payload       :: ![Closure]
      }

  | RetBig
      { info_tbl            :: !StgInfoTable
      , stack_payload       :: ![Closure]
      }

  | RetFun
      { info_tbl            :: !StgInfoTable
      , retFunType          :: RetFunType
      , retFunSize          :: Word
      , retFunFun           :: !Closure
      , retFunPayload       :: ![Closure]
      }

  |  RetBCO
      { info_tbl            :: !StgInfoTable
      , bco                 :: !Closure -- must be a BCOClosure
      , bcoArgs             :: ![Closure]
      }
  deriving (Show, Generic)

-- | Fun types according to @FunTypes.h@
-- This `Enum` must be aligned with the values in @FunTypes.h@.
data RetFunType =
      ARG_GEN     |
      ARG_GEN_BIG |
      ARG_BCO     |
      ARG_NONE    |
      ARG_N       |
      ARG_P       |
      ARG_F       |
      ARG_D       |
      ARG_L       |
      ARG_V16     |
      ARG_V32     |
      ARG_V64     |
      ARG_NN      |
      ARG_NP      |
      ARG_PN      |
      ARG_PP      |
      ARG_NNN     |
      ARG_NNP     |
      ARG_NPN     |
      ARG_NPP     |
      ARG_PNN     |
      ARG_PNP     |
      ARG_PPN     |
      ARG_PPP     |
      ARG_PPPP    |
      ARG_PPPPP   |
      ARG_PPPPPP  |
      ARG_PPPPPPP |
      ARG_PPPPPPPP
      deriving (Show, Eq, Enum, Generic)

data PrimType
  = PInt
  | PWord
  | PInt64
  | PWord64
  | PAddr
  | PFloat
  | PDouble
  deriving (Eq, Show, Generic, Ord)

data WhatNext
  = ThreadRunGHC
  | ThreadInterpret
  | ThreadKilled
  | ThreadComplete
  | WhatNextUnknownValue Word16 -- ^ Please report this as a bug
  deriving (Eq, Show, Generic, Ord)

data WhyBlocked
  = NotBlocked
  | BlockedOnMVar
  | BlockedOnMVarRead
  | BlockedOnBlackHole
  | BlockedOnRead
  | BlockedOnWrite
  | BlockedOnDelay
  | BlockedOnSTM
  | BlockedOnDoProc
  | BlockedOnCCall
  | BlockedOnCCall_Interruptible
  | BlockedOnMsgThrowTo
  | ThreadMigrating
  | WhyBlockedUnknownValue Word16 -- ^ Please report this as a bug
  deriving (Eq, Show, Generic, Ord)

data TsoFlags
  = TsoLocked
  | TsoBlockx
  | TsoInterruptible
  | TsoStoppedOnBreakpoint
  | TsoMarked
  | TsoSqueezed
  | TsoAllocLimit
  | TsoFlagsUnknownValue Word32 -- ^ Please report this as a bug
  deriving (Eq, Show, Generic, Ord)

-- | For generic code, this function returns all referenced closures.
allClosures :: GenClosure b -> [b]
allClosures (ConstrClosure {..}) = ptrArgs
allClosures (ThunkClosure {..}) = ptrArgs
allClosures (SelectorClosure {..}) = [selectee]
allClosures (IndClosure {..}) = [indirectee]
allClosures (BlackholeClosure {..}) = [indirectee]
allClosures (APClosure {..}) = fun:payload
allClosures (PAPClosure {..}) = fun:payload
allClosures (APStackClosure {..}) = fun:payload
allClosures (BCOClosure {..}) = [instrs,literals,bcoptrs]
allClosures (ArrWordsClosure {}) = []
allClosures (MutArrClosure {..}) = mccPayload
allClosures (SmallMutArrClosure {..}) = mccPayload
allClosures (MutVarClosure {..}) = [var]
allClosures (MVarClosure {..}) = [queueHead,queueTail,value]
allClosures (IOPortClosure {..}) = [queueHead,queueTail,value]
allClosures (FunClosure {..}) = ptrArgs
allClosures (BlockingQueueClosure {..}) = [link, blackHole, owner, queue]
allClosures (WeakClosure {..}) = [cfinalizers, key, value, finalizer] ++ Data.Foldable.toList weakLink
allClosures (OtherClosure {..}) = hvalues
allClosures _ = []

-- | Get the size of the top-level closure in words.
-- Includes header and payload. Does not follow pointers.
--
-- @since 8.10.1
closureSize :: Box -> Int
closureSize (Box x) = I# (closureSize# x)
