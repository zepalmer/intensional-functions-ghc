{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Conceptually, constant folding should be parameterized with the kind
of target machine to get identical behaviour during compilation time
and runtime. We cheat a little bit here...

ToDo:
   check boundaries before folding, e.g. we can fold the Float addition
   (i1 + i2) only if it results in a valid Float.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE -Wno-incomplete-uni-patterns #-}

-- | Constant Folder
module GHC.Core.Opt.ConstantFold
   ( primOpRules
   , builtinRules
   , caseRules
   )
where

#include "HsVersions.h"
#include "MachDeps.h"

import GHC.Prelude

import GHC.Driver.Ppr

import {-# SOURCE #-} GHC.Types.Id.Make ( mkPrimOpId, magicDictId, voidPrimId )

import GHC.Core
import GHC.Core.Make
import GHC.Types.Id
import GHC.Types.Literal
import GHC.Core.SimpleOpt (  exprIsConApp_maybe, exprIsLiteral_maybe )
import GHC.Builtin.PrimOps ( PrimOp(..), tagToEnumKey )
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Core.TyCon
   ( tyConDataCons_maybe, isAlgTyCon, isEnumerationTyCon
   , isNewTyCon, unwrapNewTyCon_maybe, tyConDataCons
   , tyConFamilySize )
import GHC.Core.DataCon ( dataConTagZ, dataConTyCon, dataConWrapId, dataConWorkId )
import GHC.Core.Utils  ( eqExpr, cheapEqExpr, exprIsHNF, exprType
                       , stripTicksTop, stripTicksTopT, mkTicks, stripTicksE )
import GHC.Core.Multiplicity
import GHC.Core.FVs
import GHC.Core.Type
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Name.Occurrence ( occNameFS )
import GHC.Types.Tickish
import GHC.Builtin.Names
import GHC.Data.Maybe      ( orElse )
import GHC.Types.Name ( Name, nameOccName )
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Types.Basic
import GHC.Platform
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Core.Coercion   (mkUnbranchedAxInstCo,mkSymCo,Role(..))

import Control.Applicative ( Alternative(..) )

import Control.Monad
import Data.Functor (($>))
import qualified Data.ByteString as BS
import Data.Ratio
import Data.Word
import Data.Maybe (fromMaybe)

{-
Note [Constant folding]
~~~~~~~~~~~~~~~~~~~~~~~
primOpRules generates a rewrite rule for each primop
These rules do what is often called "constant folding"
E.g. the rules for +# might say
        4 +# 5 = 9
Well, of course you'd need a lot of rules if you did it
like that, so we use a BuiltinRule instead, so that we
can match in any two literal values.  So the rule is really
more like
        (Lit x) +# (Lit y) = Lit (x+#y)
where the (+#) on the rhs is done at compile time

That is why these rules are built in here.
-}

primOpRules ::  Name -> PrimOp -> Maybe CoreRule
primOpRules nm = \case
   TagToEnumOp -> mkPrimOpRule nm 2 [ tagToEnumRule ]
   DataToTagOp -> mkPrimOpRule nm 2 [ dataToTagRule ]

   -- Int8 operations
   Int8AddOp   -> mkPrimOpRule nm 2 [ binaryLit (int8Op2 (+))
                                    , identity zeroI8
                                    , addFoldingRules Int8AddOp int8Ops
                                    ]
   Int8SubOp   -> mkPrimOpRule nm 2 [ binaryLit (int8Op2 (-))
                                    , rightIdentity zeroI8
                                    , equalArgs $> Lit zeroI8
                                    , subFoldingRules Int8SubOp int8Ops
                                    ]
   Int8MulOp   -> mkPrimOpRule nm 2 [ binaryLit (int8Op2 (*))
                                    , zeroElem
                                    , identity oneI8
                                    , mulFoldingRules Int8MulOp int8Ops
                                    ]
   Int8QuotOp  -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (int8Op2 quot)
                                    , leftZero
                                    , rightIdentity oneI8
                                    , equalArgs $> Lit oneI8 ]
   Int8RemOp   -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (int8Op2 rem)
                                    , leftZero
                                    , oneLit 1 $> Lit zeroI8
                                    , equalArgs $> Lit zeroI8 ]
   Int8NegOp   -> mkPrimOpRule nm 1 [ unaryLit negOp
                                    , semiInversePrimOp Int8NegOp ]
   Int8SllOp   -> mkPrimOpRule nm 2 [ shiftRule LitNumInt8 (const shiftL)
                                    , rightIdentity zeroI8 ]
   Int8SraOp   -> mkPrimOpRule nm 2 [ shiftRule LitNumInt8 (const shiftR)
                                    , rightIdentity zeroI8 ]
   Int8SrlOp   -> mkPrimOpRule nm 2 [ shiftRule LitNumInt8 $ const $ shiftRightLogical @Word8
                                    , rightIdentity zeroI8 ]

   -- Word8 operations
   Word8AddOp  -> mkPrimOpRule nm 2 [ binaryLit (word8Op2 (+))
                                    , identity zeroW8
                                    , addFoldingRules Word8AddOp word8Ops
                                    ]
   Word8SubOp  -> mkPrimOpRule nm 2 [ binaryLit (word8Op2 (-))
                                    , rightIdentity zeroW8
                                    , equalArgs $> Lit zeroW8
                                    , subFoldingRules Word8SubOp word8Ops
                                    ]
   Word8MulOp  -> mkPrimOpRule nm 2 [ binaryLit (word8Op2 (*))
                                    , identity oneW8
                                    , mulFoldingRules Word8MulOp word8Ops
                                    ]
   Word8QuotOp -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (word8Op2 quot)
                                    , rightIdentity oneW8 ]
   Word8RemOp  -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (word8Op2 rem)
                                    , leftZero
                                    , oneLit 1 $> Lit zeroW8
                                    , equalArgs $> Lit zeroW8 ]
   Word8AndOp  -> mkPrimOpRule nm 2 [ binaryLit (word8Op2 (.&.))
                                    , idempotent
                                    , zeroElem
                                    , sameArgIdempotentCommut Word8AndOp
                                    ]
   Word8OrOp   -> mkPrimOpRule nm 2 [ binaryLit (word8Op2 (.|.))
                                    , idempotent
                                    , identity zeroW8
                                    , sameArgIdempotentCommut Word8OrOp
                                    ]
   Word8XorOp  -> mkPrimOpRule nm 2 [ binaryLit (word8Op2 xor)
                                    , identity zeroW8
                                    , equalArgs $> Lit zeroW8 ]
   Word8NotOp  -> mkPrimOpRule nm 1 [ unaryLit complementOp
                                    , semiInversePrimOp Word8NotOp ]
   Word8SllOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumWord (const shiftL) ]
   Word8SrlOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumWord $ const $ shiftRightLogical @Word8 ]


   -- Int16 operations
   Int16AddOp  -> mkPrimOpRule nm 2 [ binaryLit (int16Op2 (+))
                                    , identity zeroI16
                                    , addFoldingRules Int16AddOp int16Ops
                                    ]
   Int16SubOp  -> mkPrimOpRule nm 2 [ binaryLit (int16Op2 (-))
                                    , rightIdentity zeroI16
                                    , equalArgs $> Lit zeroI16
                                    , subFoldingRules Int16SubOp int16Ops
                                    ]
   Int16MulOp  -> mkPrimOpRule nm 2 [ binaryLit (int16Op2 (*))
                                    , zeroElem
                                    , identity oneI16
                                    , mulFoldingRules Int16MulOp int16Ops
                                    ]
   Int16QuotOp -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (int16Op2 quot)
                                    , leftZero
                                    , rightIdentity oneI16
                                    , equalArgs $> Lit oneI16 ]
   Int16RemOp  -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (int16Op2 rem)
                                    , leftZero
                                    , oneLit 1 $> Lit zeroI16
                                    , equalArgs $> Lit zeroI16 ]
   Int16NegOp  -> mkPrimOpRule nm 1 [ unaryLit negOp
                                    , semiInversePrimOp Int16NegOp ]
   Int16SllOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumInt16 (const shiftL)
                                    , rightIdentity zeroI16 ]
   Int16SraOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumInt16 (const shiftR)
                                    , rightIdentity zeroI16 ]
   Int16SrlOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumInt16 $ const $ shiftRightLogical @Word16
                                    , rightIdentity zeroI16 ]

   -- Word16 operations
   Word16AddOp -> mkPrimOpRule nm 2 [ binaryLit (word16Op2 (+))
                                    , identity zeroW16
                                    , addFoldingRules Word16AddOp word16Ops
                                    ]
   Word16SubOp -> mkPrimOpRule nm 2 [ binaryLit (word16Op2 (-))
                                    , rightIdentity zeroW16
                                    , equalArgs $> Lit zeroW16
                                    , subFoldingRules Word16SubOp word16Ops
                                    ]
   Word16MulOp -> mkPrimOpRule nm 2 [ binaryLit (word16Op2 (*))
                                    , identity oneW16
                                    , mulFoldingRules Word16MulOp word16Ops
                                    ]
   Word16QuotOp-> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (word16Op2 quot)
                                    , rightIdentity oneW16 ]
   Word16RemOp -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (word16Op2 rem)
                                    , leftZero
                                    , oneLit 1 $> Lit zeroW16
                                    , equalArgs $> Lit zeroW16 ]
   Word16AndOp -> mkPrimOpRule nm 2 [ binaryLit (word16Op2 (.&.))
                                    , idempotent
                                    , zeroElem
                                    , sameArgIdempotentCommut Word16AndOp
                                    ]
   Word16OrOp  -> mkPrimOpRule nm 2 [ binaryLit (word16Op2 (.|.))
                                    , idempotent
                                    , identity zeroW16
                                    , sameArgIdempotentCommut Word16OrOp
                                    ]
   Word16XorOp -> mkPrimOpRule nm 2 [ binaryLit (word16Op2 xor)
                                    , identity zeroW16
                                    , equalArgs $> Lit zeroW16 ]
   Word16NotOp -> mkPrimOpRule nm 1 [ unaryLit complementOp
                                    , semiInversePrimOp Word16NotOp ]
   Word16SllOp -> mkPrimOpRule nm 2 [ shiftRule LitNumWord (const shiftL) ]
   Word16SrlOp -> mkPrimOpRule nm 2 [ shiftRule LitNumWord $ const $ shiftRightLogical @Word16 ]


   -- Int32 operations
   Int32AddOp  -> mkPrimOpRule nm 2 [ binaryLit (int32Op2 (+))
                                    , identity zeroI32
                                    , addFoldingRules Int32AddOp int32Ops
                                    ]
   Int32SubOp  -> mkPrimOpRule nm 2 [ binaryLit (int32Op2 (-))
                                    , rightIdentity zeroI32
                                    , equalArgs $> Lit zeroI32
                                    , subFoldingRules Int32SubOp int32Ops
                                    ]
   Int32MulOp  -> mkPrimOpRule nm 2 [ binaryLit (int32Op2 (*))
                                    , zeroElem
                                    , identity oneI32
                                    , mulFoldingRules Int32MulOp int32Ops
                                    ]
   Int32QuotOp -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (int32Op2 quot)
                                    , leftZero
                                    , rightIdentity oneI32
                                    , equalArgs $> Lit oneI32 ]
   Int32RemOp  -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (int32Op2 rem)
                                    , leftZero
                                    , oneLit 1 $> Lit zeroI32
                                    , equalArgs $> Lit zeroI32 ]
   Int32NegOp  -> mkPrimOpRule nm 1 [ unaryLit negOp
                                    , semiInversePrimOp Int32NegOp ]
   Int32SllOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumInt32 (const shiftL)
                                    , rightIdentity zeroI32 ]
   Int32SraOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumInt32 (const shiftR)
                                    , rightIdentity zeroI32 ]
   Int32SrlOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumInt32 $ const $ shiftRightLogical @Word32
                                    , rightIdentity zeroI32 ]

   -- Word32 operations
   Word32AddOp -> mkPrimOpRule nm 2 [ binaryLit (word32Op2 (+))
                                    , identity zeroW32
                                    , addFoldingRules Word32AddOp word32Ops
                                    ]
   Word32SubOp -> mkPrimOpRule nm 2 [ binaryLit (word32Op2 (-))
                                    , rightIdentity zeroW32
                                    , equalArgs $> Lit zeroW32
                                    , subFoldingRules Word32SubOp word32Ops
                                    ]
   Word32MulOp -> mkPrimOpRule nm 2 [ binaryLit (word32Op2 (*))
                                    , identity oneW32
                                    , mulFoldingRules Word32MulOp word32Ops
                                    ]
   Word32QuotOp-> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (word32Op2 quot)
                                    , rightIdentity oneW32 ]
   Word32RemOp -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (word32Op2 rem)
                                    , leftZero
                                    , oneLit 1 $> Lit zeroW32
                                    , equalArgs $> Lit zeroW32 ]
   Word32AndOp -> mkPrimOpRule nm 2 [ binaryLit (word32Op2 (.&.))
                                    , idempotent
                                    , zeroElem
                                    , sameArgIdempotentCommut Word32AndOp
                                    ]
   Word32OrOp  -> mkPrimOpRule nm 2 [ binaryLit (word32Op2 (.|.))
                                    , idempotent
                                    , identity zeroW32
                                    , sameArgIdempotentCommut Word32OrOp
                                    ]
   Word32XorOp -> mkPrimOpRule nm 2 [ binaryLit (word32Op2 xor)
                                    , identity zeroW32
                                    , equalArgs $> Lit zeroW32 ]
   Word32NotOp -> mkPrimOpRule nm 1 [ unaryLit complementOp
                                    , semiInversePrimOp Word32NotOp ]
   Word32SllOp -> mkPrimOpRule nm 2 [ shiftRule LitNumWord (const shiftL) ]
   Word32SrlOp -> mkPrimOpRule nm 2 [ shiftRule LitNumWord $ const $ shiftRightLogical @Word32 ]

#if WORD_SIZE_IN_BITS < 64
   -- Int64 operations
   Int64AddOp  -> mkPrimOpRule nm 2 [ binaryLit (int64Op2 (+))
                                    , identity zeroI64
                                    , addFoldingRules Int64AddOp int64Ops
                                    ]
   Int64SubOp  -> mkPrimOpRule nm 2 [ binaryLit (int64Op2 (-))
                                    , rightIdentity zeroI64
                                    , equalArgs $> Lit zeroI64
                                    , subFoldingRules Int64SubOp int64Ops
                                    ]
   Int64MulOp  -> mkPrimOpRule nm 2 [ binaryLit (int64Op2 (*))
                                    , zeroElem
                                    , identity oneI64
                                    , mulFoldingRules Int64MulOp int64Ops
                                    ]
   Int64QuotOp -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (int64Op2 quot)
                                    , leftZero
                                    , rightIdentity oneI64
                                    , equalArgs $> Lit oneI64 ]
   Int64RemOp  -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (int64Op2 rem)
                                    , leftZero
                                    , oneLit 1 $> Lit zeroI64
                                    , equalArgs $> Lit zeroI64 ]
   Int64NegOp  -> mkPrimOpRule nm 1 [ unaryLit negOp
                                    , semiInversePrimOp Int64NegOp ]
   Int64SllOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumInt64 (const shiftL)
                                    , rightIdentity zeroI64 ]
   Int64SraOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumInt64 (const shiftR)
                                    , rightIdentity zeroI64 ]
   Int64SrlOp  -> mkPrimOpRule nm 2 [ shiftRule LitNumInt64 $ const $ shiftRightLogical @Word64
                                    , rightIdentity zeroI64 ]

   -- Word64 operations
   Word64AddOp -> mkPrimOpRule nm 2 [ binaryLit (word64Op2 (+))
                                    , identity zeroW64
                                    , addFoldingRules Word64AddOp word64Ops
                                    ]
   Word64SubOp -> mkPrimOpRule nm 2 [ binaryLit (word64Op2 (-))
                                    , rightIdentity zeroW64
                                    , equalArgs $> Lit zeroW64
                                    , subFoldingRules Word64SubOp word64Ops
                                    ]
   Word64MulOp -> mkPrimOpRule nm 2 [ binaryLit (word64Op2 (*))
                                    , identity oneW64
                                    , mulFoldingRules Word64MulOp word64Ops
                                    ]
   Word64QuotOp-> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (word64Op2 quot)
                                    , rightIdentity oneW64 ]
   Word64RemOp -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (word64Op2 rem)
                                    , leftZero
                                    , oneLit 1 $> Lit zeroW64
                                    , equalArgs $> Lit zeroW64 ]
   Word64AndOp -> mkPrimOpRule nm 2 [ binaryLit (word64Op2 (.&.))
                                    , idempotent
                                    , zeroElem
                                    , sameArgIdempotentCommut Word64AndOp
                                    ]
   Word64OrOp  -> mkPrimOpRule nm 2 [ binaryLit (word64Op2 (.|.))
                                    , idempotent
                                    , identity zeroW64
                                    , sameArgIdempotentCommut Word64OrOp
                                    ]
   Word64XorOp -> mkPrimOpRule nm 2 [ binaryLit (word64Op2 xor)
                                    , identity zeroW64
                                    , equalArgs $> Lit zeroW64 ]
   Word64NotOp -> mkPrimOpRule nm 1 [ unaryLit complementOp
                                    , semiInversePrimOp Word64NotOp ]
   Word64SllOp -> mkPrimOpRule nm 2 [ shiftRule LitNumWord64 (const shiftL) ]
   Word64SrlOp -> mkPrimOpRule nm 2 [ shiftRule LitNumWord64 $ const $ shiftRightLogical @Word64 ]
#endif

   -- Int operations
   IntAddOp    -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (+))
                                    , identityPlatform zeroi
                                    , addFoldingRules IntAddOp intOps
                                    ]
   IntSubOp    -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (-))
                                    , rightIdentityPlatform zeroi
                                    , equalArgs >> retLit zeroi
                                    , subFoldingRules IntSubOp intOps
                                    ]
   IntAddCOp   -> mkPrimOpRule nm 2 [ binaryLit (intOpC2 (+))
                                    , identityCPlatform zeroi ]
   IntSubCOp   -> mkPrimOpRule nm 2 [ binaryLit (intOpC2 (-))
                                    , rightIdentityCPlatform zeroi
                                    , equalArgs >> retLitNoC zeroi ]
   IntMulOp    -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (*))
                                    , zeroElem
                                    , identityPlatform onei
                                    , mulFoldingRules IntMulOp intOps
                                    ]
   IntQuotOp   -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (intOp2 quot)
                                    , leftZero
                                    , rightIdentityPlatform onei
                                    , equalArgs >> retLit onei ]
   IntRemOp    -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (intOp2 rem)
                                    , leftZero
                                    , oneLit 1 >> retLit zeroi
                                    , equalArgs >> retLit zeroi ]
   IntAndOp    -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (.&.))
                                    , idempotent
                                    , zeroElem
                                    , sameArgIdempotentCommut IntAndOp
                                    ]
   IntOrOp     -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (.|.))
                                    , idempotent
                                    , identityPlatform zeroi
                                    , sameArgIdempotentCommut IntOrOp
                                    ]
   IntXorOp    -> mkPrimOpRule nm 2 [ binaryLit (intOp2 xor)
                                    , identityPlatform zeroi
                                    , equalArgs >> retLit zeroi ]
   IntNotOp    -> mkPrimOpRule nm 1 [ unaryLit complementOp
                                    , semiInversePrimOp IntNotOp ]
   IntNegOp    -> mkPrimOpRule nm 1 [ unaryLit negOp
                                    , semiInversePrimOp IntNegOp ]
   IntSllOp    -> mkPrimOpRule nm 2 [ shiftRule LitNumInt (const shiftL)
                                    , rightIdentityPlatform zeroi ]
   IntSraOp    -> mkPrimOpRule nm 2 [ shiftRule LitNumInt (const shiftR)
                                    , rightIdentityPlatform zeroi ]
   IntSrlOp    -> mkPrimOpRule nm 2 [ shiftRule LitNumInt shiftRightLogicalNative
                                    , rightIdentityPlatform zeroi ]

   -- Word operations
   WordAddOp   -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (+))
                                    , identityPlatform zerow
                                    , addFoldingRules WordAddOp wordOps
                                    ]
   WordSubOp   -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (-))
                                    , rightIdentityPlatform zerow
                                    , equalArgs >> retLit zerow
                                    , subFoldingRules WordSubOp wordOps
                                    ]
   WordAddCOp  -> mkPrimOpRule nm 2 [ binaryLit (wordOpC2 (+))
                                    , identityCPlatform zerow ]
   WordSubCOp  -> mkPrimOpRule nm 2 [ binaryLit (wordOpC2 (-))
                                    , rightIdentityCPlatform zerow
                                    , equalArgs >> retLitNoC zerow ]
   WordMulOp   -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (*))
                                    , identityPlatform onew
                                    , mulFoldingRules WordMulOp wordOps
                                    ]
   WordQuotOp  -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (wordOp2 quot)
                                    , rightIdentityPlatform onew ]
   WordRemOp   -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (wordOp2 rem)
                                    , leftZero
                                    , oneLit 1 >> retLit zerow
                                    , equalArgs >> retLit zerow ]
   WordAndOp   -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (.&.))
                                    , idempotent
                                    , zeroElem
                                    , sameArgIdempotentCommut WordAndOp
                                    ]
   WordOrOp    -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (.|.))
                                    , idempotent
                                    , identityPlatform zerow
                                    , sameArgIdempotentCommut WordOrOp
                                    ]
   WordXorOp   -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 xor)
                                    , identityPlatform zerow
                                    , equalArgs >> retLit zerow ]
   WordNotOp   -> mkPrimOpRule nm 1 [ unaryLit complementOp
                                    , semiInversePrimOp WordNotOp ]
   WordSllOp   -> mkPrimOpRule nm 2 [ shiftRule LitNumWord (const shiftL) ]
   WordSrlOp   -> mkPrimOpRule nm 2 [ shiftRule LitNumWord shiftRightLogicalNative ]

   -- coercions

   Int8ToIntOp    -> mkPrimOpRule nm 1 [ liftLitPlatform convertToIntLit ]
   Int16ToIntOp   -> mkPrimOpRule nm 1 [ liftLitPlatform convertToIntLit ]
   Int32ToIntOp   -> mkPrimOpRule nm 1 [ liftLitPlatform convertToIntLit ]
#if WORD_SIZE_IN_BITS < 64
   Int64ToIntOp   -> mkPrimOpRule nm 1 [ liftLitPlatform convertToIntLit ]
#endif
   IntToInt8Op    -> mkPrimOpRule nm 1 [ liftLit narrowInt8Lit
                                       , semiInversePrimOp Int8ToIntOp
                                       , narrowSubsumesAnd IntAndOp IntToInt8Op 8 ]
   IntToInt16Op   -> mkPrimOpRule nm 1 [ liftLit narrowInt16Lit
                                       , semiInversePrimOp Int16ToIntOp
                                       , narrowSubsumesAnd IntAndOp IntToInt16Op 16 ]
   IntToInt32Op   -> mkPrimOpRule nm 1 [ liftLit narrowInt32Lit
                                       , semiInversePrimOp Int32ToIntOp
                                       , narrowSubsumesAnd IntAndOp IntToInt32Op 32 ]
#if WORD_SIZE_IN_BITS < 64
   IntToInt64Op   -> mkPrimOpRule nm 1 [ liftLit narrowInt64Lit ]
#endif

   Word8ToWordOp  -> mkPrimOpRule nm 1 [ liftLitPlatform convertToWordLit
                                       , extendNarrowPassthrough WordToWord8Op 0xFF
                                       ]
   Word16ToWordOp -> mkPrimOpRule nm 1 [ liftLitPlatform convertToWordLit
                                       , extendNarrowPassthrough WordToWord16Op 0xFFFF
                                       ]
   Word32ToWordOp -> mkPrimOpRule nm 1 [ liftLitPlatform convertToWordLit
                                       , extendNarrowPassthrough WordToWord32Op 0xFFFFFFFF
                                       ]
#if WORD_SIZE_IN_BITS < 64
   Word64ToWordOp -> mkPrimOpRule nm 1 [ liftLitPlatform convertToWordLit ]
#endif

   WordToWord8Op  -> mkPrimOpRule nm 1 [ liftLit narrowWord8Lit
                                       , semiInversePrimOp Word8ToWordOp
                                       , narrowSubsumesAnd WordAndOp WordToWord8Op 8 ]
   WordToWord16Op -> mkPrimOpRule nm 1 [ liftLit narrowWord16Lit
                                       , semiInversePrimOp Word16ToWordOp
                                       , narrowSubsumesAnd WordAndOp WordToWord16Op 16 ]
   WordToWord32Op -> mkPrimOpRule nm 1 [ liftLit narrowWord32Lit
                                       , semiInversePrimOp Word32ToWordOp
                                       , narrowSubsumesAnd WordAndOp WordToWord32Op 32 ]
#if WORD_SIZE_IN_BITS < 64
   WordToWord64Op -> mkPrimOpRule nm 1 [ liftLit narrowWord64Lit ]
#endif

   Word8ToInt8Op  -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumInt8)
                                       , semiInversePrimOp Int8ToWord8Op ]
   Int8ToWord8Op  -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumWord8)
                                       , semiInversePrimOp Word8ToInt8Op ]
   Word16ToInt16Op-> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumInt16)
                                       , semiInversePrimOp Int16ToWord16Op ]
   Int16ToWord16Op-> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumWord16)
                                       , semiInversePrimOp Word16ToInt16Op ]
   Word32ToInt32Op-> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumInt32)
                                       , semiInversePrimOp Int32ToWord32Op ]
   Int32ToWord32Op-> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumWord32)
                                       , semiInversePrimOp Word32ToInt32Op ]
#if WORD_SIZE_IN_BITS < 64
   Word64ToInt64Op-> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumInt64)
                                       , semiInversePrimOp Int64ToWord64Op ]
   Int64ToWord64Op-> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumWord64)
                                       , semiInversePrimOp Word64ToInt64Op ]
#endif

   WordToIntOp    -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumInt)
                                       , semiInversePrimOp IntToWordOp ]
   IntToWordOp    -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumCoerce LitNumWord)
                                       , semiInversePrimOp WordToIntOp ]

   Narrow8IntOp   -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumNarrow LitNumInt8)
                                       , subsumedByPrimOp Narrow8IntOp
                                       , Narrow8IntOp `subsumesPrimOp` Narrow16IntOp
                                       , Narrow8IntOp `subsumesPrimOp` Narrow32IntOp
                                       , narrowSubsumesAnd IntAndOp Narrow8IntOp 8 ]
   Narrow16IntOp  -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumNarrow LitNumInt16)
                                       , subsumedByPrimOp Narrow8IntOp
                                       , subsumedByPrimOp Narrow16IntOp
                                       , Narrow16IntOp `subsumesPrimOp` Narrow32IntOp
                                       , narrowSubsumesAnd IntAndOp Narrow16IntOp 16 ]
   Narrow32IntOp  -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumNarrow LitNumInt32)
                                       , subsumedByPrimOp Narrow8IntOp
                                       , subsumedByPrimOp Narrow16IntOp
                                       , subsumedByPrimOp Narrow32IntOp
                                       , removeOp32
                                       , narrowSubsumesAnd IntAndOp Narrow32IntOp 32 ]
   Narrow8WordOp  -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumNarrow LitNumWord8)
                                       , subsumedByPrimOp Narrow8WordOp
                                       , Narrow8WordOp `subsumesPrimOp` Narrow16WordOp
                                       , Narrow8WordOp `subsumesPrimOp` Narrow32WordOp
                                       , narrowSubsumesAnd WordAndOp Narrow8WordOp 8 ]
   Narrow16WordOp -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumNarrow LitNumWord16)
                                       , subsumedByPrimOp Narrow8WordOp
                                       , subsumedByPrimOp Narrow16WordOp
                                       , Narrow16WordOp `subsumesPrimOp` Narrow32WordOp
                                       , narrowSubsumesAnd WordAndOp Narrow16WordOp 16 ]
   Narrow32WordOp -> mkPrimOpRule nm 1 [ liftLitPlatform (litNumNarrow LitNumWord32)
                                       , subsumedByPrimOp Narrow8WordOp
                                       , subsumedByPrimOp Narrow16WordOp
                                       , subsumedByPrimOp Narrow32WordOp
                                       , removeOp32
                                       , narrowSubsumesAnd WordAndOp Narrow32WordOp 32 ]

   OrdOp          -> mkPrimOpRule nm 1 [ liftLit charToIntLit
                                       , semiInversePrimOp ChrOp ]
   ChrOp          -> mkPrimOpRule nm 1 [ do [Lit lit] <- getArgs
                                            guard (litFitsInChar lit)
                                            liftLit intToCharLit
                                       , semiInversePrimOp OrdOp ]
   FloatToIntOp    -> mkPrimOpRule nm 1 [ liftLit floatToIntLit ]
   IntToFloatOp    -> mkPrimOpRule nm 1 [ liftLit intToFloatLit ]
   DoubleToIntOp   -> mkPrimOpRule nm 1 [ liftLit doubleToIntLit ]
   IntToDoubleOp   -> mkPrimOpRule nm 1 [ liftLit intToDoubleLit ]
   -- SUP: Not sure what the standard says about precision in the following 2 cases
   FloatToDoubleOp -> mkPrimOpRule nm 1 [ liftLit floatToDoubleLit ]
   DoubleToFloatOp -> mkPrimOpRule nm 1 [ liftLit doubleToFloatLit ]

   -- Float
   FloatAddOp        -> mkPrimOpRule nm 2 [ binaryLit (floatOp2 (+))
                                          , identity zerof ]
   FloatSubOp        -> mkPrimOpRule nm 2 [ binaryLit (floatOp2 (-))
                                          , rightIdentity zerof ]
   FloatMulOp        -> mkPrimOpRule nm 2 [ binaryLit (floatOp2 (*))
                                          , identity onef
                                          , strengthReduction twof FloatAddOp  ]
             -- zeroElem zerof doesn't hold because of NaN
   FloatDivOp        -> mkPrimOpRule nm 2 [ guardFloatDiv >> binaryLit (floatOp2 (/))
                                          , rightIdentity onef ]
   FloatNegOp        -> mkPrimOpRule nm 1 [ unaryLit negOp
                                          , semiInversePrimOp FloatNegOp ]
   FloatDecode_IntOp -> mkPrimOpRule nm 1 [ unaryLit floatDecodeOp ]

   -- Double
   DoubleAddOp          -> mkPrimOpRule nm 2 [ binaryLit (doubleOp2 (+))
                                             , identity zerod ]
   DoubleSubOp          -> mkPrimOpRule nm 2 [ binaryLit (doubleOp2 (-))
                                             , rightIdentity zerod ]
   DoubleMulOp          -> mkPrimOpRule nm 2 [ binaryLit (doubleOp2 (*))
                                             , identity oned
                                             , strengthReduction twod DoubleAddOp  ]
              -- zeroElem zerod doesn't hold because of NaN
   DoubleDivOp          -> mkPrimOpRule nm 2 [ guardDoubleDiv >> binaryLit (doubleOp2 (/))
                                             , rightIdentity oned ]
   DoubleNegOp          -> mkPrimOpRule nm 1 [ unaryLit negOp
                                             , semiInversePrimOp DoubleNegOp ]
   DoubleDecode_Int64Op -> mkPrimOpRule nm 1 [ unaryLit doubleDecodeOp ]

   -- Relational operators

   IntEqOp    -> mkRelOpRule nm (==) [ litEq True ]
   IntNeOp    -> mkRelOpRule nm (/=) [ litEq False ]
   CharEqOp   -> mkRelOpRule nm (==) [ litEq True ]
   CharNeOp   -> mkRelOpRule nm (/=) [ litEq False ]

   IntGtOp    -> mkRelOpRule nm (>)  [ boundsCmp Gt ]
   IntGeOp    -> mkRelOpRule nm (>=) [ boundsCmp Ge ]
   IntLeOp    -> mkRelOpRule nm (<=) [ boundsCmp Le ]
   IntLtOp    -> mkRelOpRule nm (<)  [ boundsCmp Lt ]

   CharGtOp   -> mkRelOpRule nm (>)  [ boundsCmp Gt ]
   CharGeOp   -> mkRelOpRule nm (>=) [ boundsCmp Ge ]
   CharLeOp   -> mkRelOpRule nm (<=) [ boundsCmp Le ]
   CharLtOp   -> mkRelOpRule nm (<)  [ boundsCmp Lt ]

   FloatGtOp  -> mkFloatingRelOpRule nm (>)
   FloatGeOp  -> mkFloatingRelOpRule nm (>=)
   FloatLeOp  -> mkFloatingRelOpRule nm (<=)
   FloatLtOp  -> mkFloatingRelOpRule nm (<)
   FloatEqOp  -> mkFloatingRelOpRule nm (==)
   FloatNeOp  -> mkFloatingRelOpRule nm (/=)

   DoubleGtOp -> mkFloatingRelOpRule nm (>)
   DoubleGeOp -> mkFloatingRelOpRule nm (>=)
   DoubleLeOp -> mkFloatingRelOpRule nm (<=)
   DoubleLtOp -> mkFloatingRelOpRule nm (<)
   DoubleEqOp -> mkFloatingRelOpRule nm (==)
   DoubleNeOp -> mkFloatingRelOpRule nm (/=)

   WordGtOp   -> mkRelOpRule nm (>)  [ boundsCmp Gt ]
   WordGeOp   -> mkRelOpRule nm (>=) [ boundsCmp Ge ]
   WordLeOp   -> mkRelOpRule nm (<=) [ boundsCmp Le ]
   WordLtOp   -> mkRelOpRule nm (<)  [ boundsCmp Lt ]
   WordEqOp   -> mkRelOpRule nm (==) [ litEq True ]
   WordNeOp   -> mkRelOpRule nm (/=) [ litEq False ]

   AddrAddOp  -> mkPrimOpRule nm 2 [ rightIdentityPlatform zeroi ]

   SeqOp      -> mkPrimOpRule nm 4 [ seqRule ]
   SparkOp    -> mkPrimOpRule nm 4 [ sparkRule ]

   _          -> Nothing

{-
************************************************************************
*                                                                      *
\subsection{Doing the business}
*                                                                      *
************************************************************************
-}

-- useful shorthands
mkPrimOpRule :: Name -> Int -> [RuleM CoreExpr] -> Maybe CoreRule
mkPrimOpRule nm arity rules = Just $ mkBasicRule nm arity (msum rules)

mkRelOpRule :: Name -> (forall a . Ord a => a -> a -> Bool)
            -> [RuleM CoreExpr] -> Maybe CoreRule
mkRelOpRule nm cmp extra
  = mkPrimOpRule nm 2 $
    binaryCmpLit cmp : equal_rule : extra
  where
        -- x `cmp` x does not depend on x, so
        -- compute it for the arbitrary value 'True'
        -- and use that result
    equal_rule = do { equalArgs
                    ; platform <- getPlatform
                    ; return (if cmp True True
                              then trueValInt  platform
                              else falseValInt platform) }

{- Note [Rules for floating-point comparisons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need different rules for floating-point values because for floats
it is not true that x = x (for NaNs); so we do not want the equal_rule
rule that mkRelOpRule uses.

Note also that, in the case of equality/inequality, we do /not/
want to switch to a case-expression.  For example, we do not want
to convert
   case (eqFloat# x 3.8#) of
     True -> this
     False -> that
to
  case x of
    3.8#::Float# -> this
    _            -> that
See #9238.  Reason: comparing floating-point values for equality
delicate, and we don't want to implement that delicacy in the code for
case expressions.  So we make it an invariant of Core that a case
expression never scrutinises a Float# or Double#.

This transformation is what the litEq rule does;
see Note [The litEq rule: converting equality to case].
So we /refrain/ from using litEq for mkFloatingRelOpRule.
-}

mkFloatingRelOpRule :: Name -> (forall a . Ord a => a -> a -> Bool)
                    -> Maybe CoreRule
-- See Note [Rules for floating-point comparisons]
mkFloatingRelOpRule nm cmp
  = mkPrimOpRule nm 2 [binaryCmpLit cmp]

-- common constants
zeroi, onei, zerow, onew :: Platform -> Literal
zeroi platform = mkLitInt  platform 0
onei  platform = mkLitInt  platform 1
zerow platform = mkLitWord platform 0
onew  platform = mkLitWord platform 1

zeroI8, oneI8, zeroW8, oneW8 :: Literal
zeroI8 = mkLitInt8  0
oneI8  = mkLitInt8  1
zeroW8 = mkLitWord8 0
oneW8  = mkLitWord8 1

zeroI16, oneI16, zeroW16, oneW16 :: Literal
zeroI16 = mkLitInt16  0
oneI16  = mkLitInt16  1
zeroW16 = mkLitWord16 0
oneW16  = mkLitWord16 1

zeroI32, oneI32, zeroW32, oneW32 :: Literal
zeroI32 = mkLitInt32  0
oneI32  = mkLitInt32  1
zeroW32 = mkLitWord32 0
oneW32  = mkLitWord32 1

#if WORD_SIZE_IN_BITS < 64
zeroI64, oneI64, zeroW64, oneW64 :: Literal
zeroI64 = mkLitInt64  0
oneI64  = mkLitInt64  1
zeroW64 = mkLitWord64 0
oneW64  = mkLitWord64 1
#endif

zerof, onef, twof, zerod, oned, twod :: Literal
zerof = mkLitFloat 0.0
onef  = mkLitFloat 1.0
twof  = mkLitFloat 2.0
zerod = mkLitDouble 0.0
oned  = mkLitDouble 1.0
twod  = mkLitDouble 2.0

cmpOp :: Platform -> (forall a . Ord a => a -> a -> Bool)
      -> Literal -> Literal -> Maybe CoreExpr
cmpOp platform cmp = go
  where
    done True  = Just $ trueValInt  platform
    done False = Just $ falseValInt platform

    -- These compares are at different types
    go (LitChar i1)   (LitChar i2)   = done (i1 `cmp` i2)
    go (LitFloat i1)  (LitFloat i2)  = done (i1 `cmp` i2)
    go (LitDouble i1) (LitDouble i2) = done (i1 `cmp` i2)
    go (LitNumber nt1 i1) (LitNumber nt2 i2)
      | nt1 /= nt2 = Nothing
      | otherwise  = done (i1 `cmp` i2)
    go _               _               = Nothing

--------------------------

negOp :: RuleOpts -> Literal -> Maybe CoreExpr  -- Negate
negOp env = \case
   (LitFloat 0.0)  -> Nothing  -- can't represent -0.0 as a Rational
   (LitFloat f)    -> Just (mkFloatVal env (-f))
   (LitDouble 0.0) -> Nothing
   (LitDouble d)   -> Just (mkDoubleVal env (-d))
   (LitNumber nt i)
      | litNumIsSigned nt -> Just (Lit (mkLitNumberWrap (roPlatform env) nt (-i)))
   _ -> Nothing

complementOp :: RuleOpts -> Literal -> Maybe CoreExpr  -- Binary complement
complementOp env (LitNumber nt i) =
   Just (Lit (mkLitNumberWrap (roPlatform env) nt (complement i)))
complementOp _      _            = Nothing

int8Op2
  :: (Integral a, Integral b)
  => (a -> b -> Integer)
  -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
int8Op2 op _ (LitNumber LitNumInt8 i1) (LitNumber LitNumInt8 i2) =
  int8Result (fromInteger i1 `op` fromInteger i2)
int8Op2 _ _ _ _ = Nothing

int16Op2
  :: (Integral a, Integral b)
  => (a -> b -> Integer)
  -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
int16Op2 op _ (LitNumber LitNumInt16 i1) (LitNumber LitNumInt16 i2) =
  int16Result (fromInteger i1 `op` fromInteger i2)
int16Op2 _ _ _ _ = Nothing

int32Op2
  :: (Integral a, Integral b)
  => (a -> b -> Integer)
  -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
int32Op2 op _ (LitNumber LitNumInt32 i1) (LitNumber LitNumInt32 i2) =
  int32Result (fromInteger i1 `op` fromInteger i2)
int32Op2 _ _ _ _ = Nothing

#if WORD_SIZE_IN_BITS < 64
int64Op2
  :: (Integral a, Integral b)
  => (a -> b -> Integer)
  -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
int64Op2 op _ (LitNumber LitNumInt64 i1) (LitNumber LitNumInt64 i2) =
  int64Result (fromInteger i1 `op` fromInteger i2)
int64Op2 _ _ _ _ = Nothing
#endif

intOp2 :: (Integral a, Integral b)
       => (a -> b -> Integer)
       -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
intOp2 = intOp2' . const

intOp2' :: (Integral a, Integral b)
        => (RuleOpts -> a -> b -> Integer)
        -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
intOp2' op env (LitNumber LitNumInt i1) (LitNumber LitNumInt i2) =
  let o = op env
  in  intResult (roPlatform env) (fromInteger i1 `o` fromInteger i2)
intOp2' _ _ _ _ = Nothing

intOpC2 :: (Integral a, Integral b)
        => (a -> b -> Integer)
        -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
intOpC2 op env (LitNumber LitNumInt i1) (LitNumber LitNumInt i2) =
  intCResult (roPlatform env) (fromInteger i1 `op` fromInteger i2)
intOpC2 _ _ _ _ = Nothing

shiftRightLogical :: forall t. (Integral t, Bits t) => Integer -> Int -> Integer
shiftRightLogical x n = fromIntegral (fromInteger x `shiftR` n :: t)

-- | Shift right, putting zeros in rather than sign-propagating as
-- 'Bits.shiftR' would do. Do this by converting to the appropriate Word
-- and back. Obviously this won't work for too-big values, but its ok as
-- we use it here.
shiftRightLogicalNative :: Platform -> Integer -> Int -> Integer
shiftRightLogicalNative platform =
    case platformWordSize platform of
      PW4 -> shiftRightLogical @Word32
      PW8 -> shiftRightLogical @Word64

--------------------------
retLit :: (Platform -> Literal) -> RuleM CoreExpr
retLit l = do platform <- getPlatform
              return $ Lit $ l platform

retLitNoC :: (Platform -> Literal) -> RuleM CoreExpr
retLitNoC l = do platform <- getPlatform
                 let lit = l platform
                 let ty = literalType lit
                 return $ mkCoreUbxTup [ty, ty] [Lit lit, Lit (zeroi platform)]

word8Op2
  :: (Integral a, Integral b)
  => (a -> b -> Integer)
  -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
word8Op2 op _ (LitNumber LitNumWord8 i1) (LitNumber LitNumWord8 i2) =
  word8Result (fromInteger i1 `op` fromInteger i2)
word8Op2 _ _ _ _ = Nothing

word16Op2
  :: (Integral a, Integral b)
  => (a -> b -> Integer)
  -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
word16Op2 op _ (LitNumber LitNumWord16 i1) (LitNumber LitNumWord16 i2) =
  word16Result (fromInteger i1 `op` fromInteger i2)
word16Op2 _ _ _ _ = Nothing

word32Op2
  :: (Integral a, Integral b)
  => (a -> b -> Integer)
  -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
word32Op2 op _ (LitNumber LitNumWord32 i1) (LitNumber LitNumWord32 i2) =
  word32Result (fromInteger i1 `op` fromInteger i2)
word32Op2 _ _ _ _ = Nothing

#if WORD_SIZE_IN_BITS < 64
word64Op2
  :: (Integral a, Integral b)
  => (a -> b -> Integer)
  -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
word64Op2 op _ (LitNumber LitNumWord64 i1) (LitNumber LitNumWord64 i2) =
  word64Result (fromInteger i1 `op` fromInteger i2)
word64Op2 _ _ _ _ = Nothing
#endif

wordOp2 :: (Integral a, Integral b)
        => (a -> b -> Integer)
        -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
wordOp2 op env (LitNumber LitNumWord w1) (LitNumber LitNumWord w2)
    = wordResult (roPlatform env) (fromInteger w1 `op` fromInteger w2)
wordOp2 _ _ _ _ = Nothing

wordOpC2 :: (Integral a, Integral b)
        => (a -> b -> Integer)
        -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
wordOpC2 op env (LitNumber LitNumWord w1) (LitNumber LitNumWord w2) =
  wordCResult (roPlatform env) (fromInteger w1 `op` fromInteger w2)
wordOpC2 _ _ _ _ = Nothing

shiftRule :: LitNumType
          -> (Platform -> Integer -> Int -> Integer)
          -> RuleM CoreExpr
-- Shifts take an Int; hence third arg of op is Int
-- Used for shift primops
--    IntSllOp, IntSraOp, IntSrlOp :: Int# -> Int# -> Int#
--    SllOp, SrlOp                 :: Word# -> Int# -> Word#
shiftRule lit_num_ty shift_op = do
  platform <- getPlatform
  [e1, Lit (LitNumber LitNumInt shift_len)] <- getArgs

  bit_size <- case litNumBitSize platform lit_num_ty of
   Nothing -> mzero
   Just bs -> pure (toInteger bs)

  case e1 of
    _ | shift_len == 0 -> pure e1

      -- See Note [Guarding against silly shifts]
    _ | shift_len < 0 || shift_len > bit_size
      -> pure $ Lit $ mkLitNumberWrap platform lit_num_ty 0
           -- Be sure to use lit_num_ty here, so we get a correctly typed zero.
           -- See #18589

    Lit (LitNumber nt x)
       | 0 < shift_len && shift_len <= bit_size
       -> ASSERT(nt == lit_num_ty)
          let op = shift_op platform
              -- Do the shift at type Integer, but shift length is Int.
              -- Using host's Int is ok even if target's Int has a different size
              -- because we test that shift_len <= bit_size (which is at most 64)
              y  = x `op` fromInteger shift_len
          in pure $ Lit $ mkLitNumberWrap platform nt y

    _ -> mzero

--------------------------
floatOp2 :: (Rational -> Rational -> Rational)
         -> RuleOpts -> Literal -> Literal
         -> Maybe (Expr CoreBndr)
floatOp2 op env (LitFloat f1) (LitFloat f2)
  = Just (mkFloatVal env (f1 `op` f2))
floatOp2 _ _ _ _ = Nothing

--------------------------
floatDecodeOp :: RuleOpts -> Literal -> Maybe CoreExpr
floatDecodeOp env (LitFloat ((decodeFloat . fromRational @Float) -> (m, e)))
  = Just $ mkCoreUbxTup [intPrimTy, intPrimTy]
                        [ mkIntVal (roPlatform env) (toInteger m)
                        , mkIntVal (roPlatform env) (toInteger e) ]
floatDecodeOp _   _
  = Nothing

--------------------------
doubleOp2 :: (Rational -> Rational -> Rational)
          -> RuleOpts -> Literal -> Literal
          -> Maybe (Expr CoreBndr)
doubleOp2 op env (LitDouble f1) (LitDouble f2)
  = Just (mkDoubleVal env (f1 `op` f2))
doubleOp2 _ _ _ _ = Nothing

--------------------------
doubleDecodeOp :: RuleOpts -> Literal -> Maybe CoreExpr
doubleDecodeOp env (LitDouble ((decodeFloat . fromRational @Double) -> (m, e)))
  = Just $ mkCoreUbxTup [iNT64Ty, intPrimTy]
                        [ Lit (mkLitINT64 (toInteger m))
                        , mkIntVal platform (toInteger e) ]
  where
    platform = roPlatform env
    (iNT64Ty, mkLitINT64)
      | platformWordSizeInBits platform < 64
      = (int64PrimTy, mkLitInt64Wrap)
      | otherwise
      = (intPrimTy  , mkLitIntWrap platform)
doubleDecodeOp _   _
  = Nothing

--------------------------
{- Note [The litEq rule: converting equality to case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This stuff turns
     n ==# 3#
into
     case n of
       3# -> True
       m  -> False

This is a Good Thing, because it allows case-of case things
to happen, and case-default absorption to happen.  For
example:

     if (n ==# 3#) || (n ==# 4#) then e1 else e2
will transform to
     case n of
       3# -> e1
       4# -> e1
       m  -> e2
(modulo the usual precautions to avoid duplicating e1)
-}

litEq :: Bool  -- True <=> equality, False <=> inequality
      -> RuleM CoreExpr
litEq is_eq = msum
  [ do [Lit lit, expr] <- getArgs
       platform <- getPlatform
       do_lit_eq platform lit expr
  , do [expr, Lit lit] <- getArgs
       platform <- getPlatform
       do_lit_eq platform lit expr ]
  where
    do_lit_eq platform lit expr = do
      guard (not (litIsLifted lit))
      return (mkWildCase expr (unrestricted $ literalType lit) intPrimTy
                    [ Alt DEFAULT      [] val_if_neq
                    , Alt (LitAlt lit) [] val_if_eq])
      where
        val_if_eq  | is_eq     = trueValInt  platform
                   | otherwise = falseValInt platform
        val_if_neq | is_eq     = falseValInt platform
                   | otherwise = trueValInt  platform


-- | Check if there is comparison with minBound or maxBound, that is
-- always true or false. For instance, an Int cannot be smaller than its
-- minBound, so we can replace such comparison with False.
boundsCmp :: Comparison -> RuleM CoreExpr
boundsCmp op = do
  platform <- getPlatform
  [a, b] <- getArgs
  liftMaybe $ mkRuleFn platform op a b

data Comparison = Gt | Ge | Lt | Le

mkRuleFn :: Platform -> Comparison -> CoreExpr -> CoreExpr -> Maybe CoreExpr
mkRuleFn platform Gt (Lit lit) _ | isMinBound platform lit = Just $ falseValInt platform
mkRuleFn platform Le (Lit lit) _ | isMinBound platform lit = Just $ trueValInt  platform
mkRuleFn platform Ge _ (Lit lit) | isMinBound platform lit = Just $ trueValInt  platform
mkRuleFn platform Lt _ (Lit lit) | isMinBound platform lit = Just $ falseValInt platform
mkRuleFn platform Ge (Lit lit) _ | isMaxBound platform lit = Just $ trueValInt  platform
mkRuleFn platform Lt (Lit lit) _ | isMaxBound platform lit = Just $ falseValInt platform
mkRuleFn platform Gt _ (Lit lit) | isMaxBound platform lit = Just $ falseValInt platform
mkRuleFn platform Le _ (Lit lit) | isMaxBound platform lit = Just $ trueValInt  platform
mkRuleFn _ _ _ _                                           = Nothing

-- | Create an Int literal expression while ensuring the given Integer is in the
-- target Int range
int8Result :: Integer -> Maybe CoreExpr
int8Result result = Just (int8Result' result)

int8Result' :: Integer -> CoreExpr
int8Result' result = Lit (mkLitInt8Wrap result)

-- | Create an Int literal expression while ensuring the given Integer is in the
-- target Int range
int16Result :: Integer -> Maybe CoreExpr
int16Result result = Just (int16Result' result)

int16Result' :: Integer -> CoreExpr
int16Result' result = Lit (mkLitInt16Wrap result)

-- | Create an Int literal expression while ensuring the given Integer is in the
-- target Int range
int32Result :: Integer -> Maybe CoreExpr
int32Result result = Just (int32Result' result)

int32Result' :: Integer -> CoreExpr
int32Result' result = Lit (mkLitInt32Wrap result)

intResult :: Platform -> Integer -> Maybe CoreExpr
intResult platform result = Just (intResult' platform result)

intResult' :: Platform -> Integer -> CoreExpr
intResult' platform result = Lit (mkLitIntWrap platform result)

-- | Create an unboxed pair of an Int literal expression, ensuring the given
-- Integer is in the target Int range and the corresponding overflow flag
-- (@0#@/@1#@) if it wasn't.
intCResult :: Platform -> Integer -> Maybe CoreExpr
intCResult platform result = Just (mkPair [Lit lit, Lit c])
  where
    mkPair = mkCoreUbxTup [intPrimTy, intPrimTy]
    (lit, b) = mkLitIntWrapC platform result
    c = if b then onei platform else zeroi platform

-- | Create a Word literal expression while ensuring the given Integer is in the
-- target Word range
word8Result :: Integer -> Maybe CoreExpr
word8Result result = Just (word8Result' result)

word8Result' :: Integer -> CoreExpr
word8Result' result = Lit (mkLitWord8Wrap result)

-- | Create a Word literal expression while ensuring the given Integer is in the
-- target Word range
word16Result :: Integer -> Maybe CoreExpr
word16Result result = Just (word16Result' result)

word16Result' :: Integer -> CoreExpr
word16Result' result = Lit (mkLitWord16Wrap result)

-- | Create a Word literal expression while ensuring the given Integer is in the
-- target Word range
word32Result :: Integer -> Maybe CoreExpr
word32Result result = Just (word32Result' result)

word32Result' :: Integer -> CoreExpr
word32Result' result = Lit (mkLitWord32Wrap result)

-- | Create a Word literal expression while ensuring the given Integer is in the
-- target Word range
wordResult :: Platform -> Integer -> Maybe CoreExpr
wordResult platform result = Just (wordResult' platform result)

wordResult' :: Platform -> Integer -> CoreExpr
wordResult' platform result = Lit (mkLitWordWrap platform result)

-- | Create an unboxed pair of a Word literal expression, ensuring the given
-- Integer is in the target Word range and the corresponding carry flag
-- (@0#@/@1#@) if it wasn't.
wordCResult :: Platform -> Integer -> Maybe CoreExpr
wordCResult platform result = Just (mkPair [Lit lit, Lit c])
  where
    mkPair = mkCoreUbxTup [wordPrimTy, intPrimTy]
    (lit, b) = mkLitWordWrapC platform result
    c = if b then onei platform else zeroi platform

#if WORD_SIZE_IN_BITS < 64
int64Result :: Integer -> Maybe CoreExpr
int64Result result = Just (int64Result' result)

int64Result' :: Integer -> CoreExpr
int64Result' result = Lit (mkLitInt64Wrap result)

word64Result :: Integer -> Maybe CoreExpr
word64Result result = Just (word64Result' result)

word64Result' :: Integer -> CoreExpr
word64Result' result = Lit (mkLitWord64Wrap result)
#endif


-- | 'ambiant (primop x) = x', but not nececesarily 'primop (ambient x) = x'.
semiInversePrimOp :: PrimOp -> RuleM CoreExpr
semiInversePrimOp primop = do
  [Var primop_id `App` e] <- getArgs
  matchPrimOpId primop primop_id
  return e

subsumesPrimOp :: PrimOp -> PrimOp -> RuleM CoreExpr
this `subsumesPrimOp` that = do
  [Var primop_id `App` e] <- getArgs
  matchPrimOpId that primop_id
  return (Var (mkPrimOpId this) `App` e)

subsumedByPrimOp :: PrimOp -> RuleM CoreExpr
subsumedByPrimOp primop = do
  [e@(Var primop_id `App` _)] <- getArgs
  matchPrimOpId primop primop_id
  return e

-- | Transform `extendWordN (narrowWordN x)` into `x .&. 0xFF..FF`
extendNarrowPassthrough :: PrimOp -> Integer -> RuleM CoreExpr
extendNarrowPassthrough narrow_primop n = do
  [Var primop_id `App` x] <- getArgs
  matchPrimOpId narrow_primop primop_id
  return (Var (mkPrimOpId WordAndOp) `App` x `App` Lit (LitNumber LitNumWord n))

-- | narrow subsumes bitwise `and` with full mask (cf #16402):
--
--       narrowN (x .&. m)
--       m .&. (2^N-1) = 2^N-1
--       ==> narrowN x
--
-- e.g.  narrow16 (x .&. 0xFFFF)
--       ==> narrow16 x
--
narrowSubsumesAnd :: PrimOp -> PrimOp -> Int -> RuleM CoreExpr
narrowSubsumesAnd and_primop narrw n = do
  [Var primop_id `App` x `App` y] <- getArgs
  matchPrimOpId and_primop primop_id
  let mask = bit n -1
      g v (Lit (LitNumber _ m)) = do
         guard (m .&. mask == mask)
         return (Var (mkPrimOpId narrw) `App` v)
      g _ _ = mzero
  g x y <|> g y x

idempotent :: RuleM CoreExpr
idempotent = do [e1, e2] <- getArgs
                guard $ cheapEqExpr e1 e2
                return e1

-- | Match
--       (op (op v e) e)
--    or (op e (op v e))
--    or (op (op e v) e)
--    or (op e (op e v))
--  and return the innermost (op v e) or (op e v).
sameArgIdempotentCommut :: PrimOp -> RuleM CoreExpr
sameArgIdempotentCommut op = do
  let is_op = \case
        BinOpApp v op' e | op == op' -> Just (v,e)
        _                            -> Nothing
  [a,b] <- getArgs
  case (a,b) of
    (is_op -> Just (e1,e2), e3)
      | cheapEqExpr e2 e3 -> return a
      | cheapEqExpr e1 e3 -> return a
    (e3, is_op -> Just (e1,e2))
      | cheapEqExpr e2 e3 -> return b
      | cheapEqExpr e1 e3 -> return b
    _ -> mzero

{-
Note [Guarding against silly shifts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this code:

  import Data.Bits( (.|.), shiftL )
  chunkToBitmap :: [Bool] -> Word32
  chunkToBitmap chunk = foldr (.|.) 0 [ 1 `shiftL` n | (True,n) <- zip chunk [0..] ]

This optimises to:
Shift.$wgo = \ (w_sCS :: GHC.Prim.Int#) (w1_sCT :: [GHC.Types.Bool]) ->
    case w1_sCT of _ {
      [] -> 0##;
      : x_aAW xs_aAX ->
        case x_aAW of _ {
          GHC.Types.False ->
            case w_sCS of wild2_Xh {
              __DEFAULT -> Shift.$wgo (GHC.Prim.+# wild2_Xh 1) xs_aAX;
              9223372036854775807 -> 0## };
          GHC.Types.True ->
            case GHC.Prim.>=# w_sCS 64 of _ {
              GHC.Types.False ->
                case w_sCS of wild3_Xh {
                  __DEFAULT ->
                    case Shift.$wgo (GHC.Prim.+# wild3_Xh 1) xs_aAX of ww_sCW { __DEFAULT ->
                      GHC.Prim.or# (GHC.Prim.narrow32Word#
                                      (GHC.Prim.uncheckedShiftL# 1## wild3_Xh))
                                   ww_sCW
                     };
                  9223372036854775807 ->
                    GHC.Prim.narrow32Word#
!!!!-->                  (GHC.Prim.uncheckedShiftL# 1## 9223372036854775807)
                };
              GHC.Types.True ->
                case w_sCS of wild3_Xh {
                  __DEFAULT -> Shift.$wgo (GHC.Prim.+# wild3_Xh 1) xs_aAX;
                  9223372036854775807 -> 0##
                } } } }

Note the massive shift on line "!!!!".  It can't happen, because we've checked
that w < 64, but the optimiser didn't spot that. We DO NOT want to constant-fold this!
Moreover, if the programmer writes (n `uncheckedShiftL` 9223372036854775807), we
can't constant fold it, but if it gets to the assembler we get
     Error: operand type mismatch for `shl'

So the best thing to do is to rewrite the shift with a call to error,
when the second arg is large. However, in general we cannot do this; consider
this case

    let x = I# (uncheckedIShiftL# n 80)
    in ...

Here x contains an invalid shift and consequently we would like to rewrite it
as follows:

    let x = I# (error "invalid shift)
    in ...

This was originally done in the fix to #16449 but this breaks the let/app
invariant (see Note [Core let/app invariant] in GHC.Core) as noted in #16742.
For the reasons discussed in Note [Checking versus non-checking primops] (in
the PrimOp module) there is no safe way rewrite the argument of I# such that
it bottoms.

Consequently we instead take advantage of the fact that large shifts are
undefined behavior (see associated documentation in primops.txt.pp) and
transform the invalid shift into an "obviously incorrect" value.

There are two cases:

- Shifting fixed-width things: the primops IntSll, Sll, etc
  These are handled by shiftRule.

  We are happy to shift by any amount up to wordSize but no more.

- Shifting Bignums (Integer, Natural): these are handled by bignum_shift.

  Here we could in principle shift by any amount, but we arbitrary
  limit the shift to 4 bits; in particular we do not want shift by a
  huge amount, which can happen in code like that above.

The two cases are more different in their code paths that is comfortable,
but that is only a historical accident.


************************************************************************
*                                                                      *
\subsection{Vaguely generic functions}
*                                                                      *
************************************************************************
-}

mkBasicRule :: Name -> Int -> RuleM CoreExpr -> CoreRule
-- Gives the Rule the same name as the primop itself
mkBasicRule op_name n_args rm
  = BuiltinRule { ru_name  = occNameFS (nameOccName op_name),
                  ru_fn    = op_name,
                  ru_nargs = n_args,
                  ru_try   = runRuleM rm }

newtype RuleM r = RuleM
  { runRuleM :: RuleOpts -> InScopeEnv -> Id -> [CoreExpr] -> Maybe r }
  deriving (Functor)

instance Applicative RuleM where
    pure x = RuleM $ \_ _ _ _ -> Just x
    (<*>) = ap

instance Monad RuleM where
  RuleM f >>= g
    = RuleM $ \env iu fn args ->
              case f env iu fn args of
                Nothing -> Nothing
                Just r  -> runRuleM (g r) env iu fn args

instance MonadFail RuleM where
    fail _ = mzero

instance Alternative RuleM where
  empty = RuleM $ \_ _ _ _ -> Nothing
  RuleM f1 <|> RuleM f2 = RuleM $ \env iu fn args ->
    f1 env iu fn args <|> f2 env iu fn args

instance MonadPlus RuleM

getPlatform :: RuleM Platform
getPlatform = roPlatform <$> getRuleOpts

getRuleOpts :: RuleM RuleOpts
getRuleOpts = RuleM $ \rule_opts _ _ _ -> Just rule_opts

getEnv :: RuleM InScopeEnv
getEnv = RuleM $ \_ env _ _ -> Just env

liftMaybe :: Maybe a -> RuleM a
liftMaybe Nothing = mzero
liftMaybe (Just x) = return x

liftLit :: (Literal -> Literal) -> RuleM CoreExpr
liftLit f = liftLitPlatform (const f)

liftLitPlatform :: (Platform -> Literal -> Literal) -> RuleM CoreExpr
liftLitPlatform f = do
  platform <- getPlatform
  [Lit lit] <- getArgs
  return $ Lit (f platform lit)

removeOp32 :: RuleM CoreExpr
removeOp32 = do
  platform <- getPlatform
  case platformWordSize platform of
    PW4 -> do
      [e] <- getArgs
      return e
    PW8 ->
      mzero

getArgs :: RuleM [CoreExpr]
getArgs = RuleM $ \_ _ _ args -> Just args

getInScopeEnv :: RuleM InScopeEnv
getInScopeEnv = RuleM $ \_ iu _ _ -> Just iu

getFunction :: RuleM Id
getFunction = RuleM $ \_ _ fn _ -> Just fn

exprIsVarApp_maybe :: InScopeEnv -> CoreExpr -> Maybe (Id,CoreArg)
exprIsVarApp_maybe env@(_, id_unf) e = case e of
  App (Var f) a -> Just (f, a)
  Var v
    | Just rhs <- expandUnfolding_maybe (id_unf v)
    -> exprIsVarApp_maybe env rhs
  _ -> Nothing

-- | Looks into the expression or its unfolding to find "App (Var f) x"
isVarApp :: InScopeEnv -> CoreExpr -> RuleM (Id,CoreArg)
isVarApp env e = case exprIsVarApp_maybe env e of
  Nothing -> mzero
  Just r  -> pure r

isLiteral :: CoreExpr -> RuleM Literal
isLiteral e = do
    env <- getInScopeEnv
    case exprIsLiteral_maybe env e of
        Nothing -> mzero
        Just l  -> pure l

isNumberLiteral :: CoreExpr -> RuleM Integer
isNumberLiteral e = isLiteral e >>= \case
  LitNumber _ x -> pure x
  _             -> mzero

isIntegerLiteral :: CoreExpr -> RuleM Integer
isIntegerLiteral e = isLiteral e >>= \case
  LitNumber LitNumInteger x -> pure x
  _                         -> mzero

isNaturalLiteral :: CoreExpr -> RuleM Integer
isNaturalLiteral e = isLiteral e >>= \case
  LitNumber LitNumNatural x -> pure x
  _                         -> mzero

isWordLiteral :: CoreExpr -> RuleM Integer
isWordLiteral e = isLiteral e >>= \case
  LitNumber LitNumWord x -> pure x
  _                      -> mzero

isIntLiteral :: CoreExpr -> RuleM Integer
isIntLiteral e = isLiteral e >>= \case
  LitNumber LitNumInt x -> pure x
  _                     -> mzero

-- return the n-th argument of this rule, if it is a literal
-- argument indices start from 0
getLiteral :: Int -> RuleM Literal
getLiteral n = RuleM $ \_ _ _ exprs -> case drop n exprs of
  (Lit l:_) -> Just l
  _ -> Nothing

unaryLit :: (RuleOpts -> Literal -> Maybe CoreExpr) -> RuleM CoreExpr
unaryLit op = do
  env <- getRuleOpts
  [Lit l] <- getArgs
  liftMaybe $ op env (convFloating env l)

binaryLit :: (RuleOpts -> Literal -> Literal -> Maybe CoreExpr) -> RuleM CoreExpr
binaryLit op = do
  env <- getRuleOpts
  [Lit l1, Lit l2] <- getArgs
  liftMaybe $ op env (convFloating env l1) (convFloating env l2)

binaryCmpLit :: (forall a . Ord a => a -> a -> Bool) -> RuleM CoreExpr
binaryCmpLit op = do
  platform <- getPlatform
  binaryLit (\_ -> cmpOp platform op)

leftIdentity :: Literal -> RuleM CoreExpr
leftIdentity id_lit = leftIdentityPlatform (const id_lit)

rightIdentity :: Literal -> RuleM CoreExpr
rightIdentity id_lit = rightIdentityPlatform (const id_lit)

identity :: Literal -> RuleM CoreExpr
identity lit = leftIdentity lit `mplus` rightIdentity lit

leftIdentityPlatform :: (Platform -> Literal) -> RuleM CoreExpr
leftIdentityPlatform id_lit = do
  platform <- getPlatform
  [Lit l1, e2] <- getArgs
  guard $ l1 == id_lit platform
  return e2

-- | Left identity rule for PrimOps like 'IntAddC' and 'WordAddC', where, in
-- addition to the result, we have to indicate that no carry/overflow occurred.
leftIdentityCPlatform :: (Platform -> Literal) -> RuleM CoreExpr
leftIdentityCPlatform id_lit = do
  platform <- getPlatform
  [Lit l1, e2] <- getArgs
  guard $ l1 == id_lit platform
  let no_c = Lit (zeroi platform)
  return (mkCoreUbxTup [exprType e2, intPrimTy] [e2, no_c])

rightIdentityPlatform :: (Platform -> Literal) -> RuleM CoreExpr
rightIdentityPlatform id_lit = do
  platform <- getPlatform
  [e1, Lit l2] <- getArgs
  guard $ l2 == id_lit platform
  return e1

-- | Right identity rule for PrimOps like 'IntSubC' and 'WordSubC', where, in
-- addition to the result, we have to indicate that no carry/overflow occurred.
rightIdentityCPlatform :: (Platform -> Literal) -> RuleM CoreExpr
rightIdentityCPlatform id_lit = do
  platform <- getPlatform
  [e1, Lit l2] <- getArgs
  guard $ l2 == id_lit platform
  let no_c = Lit (zeroi platform)
  return (mkCoreUbxTup [exprType e1, intPrimTy] [e1, no_c])

identityPlatform :: (Platform -> Literal) -> RuleM CoreExpr
identityPlatform lit =
  leftIdentityPlatform lit `mplus` rightIdentityPlatform lit

-- | Identity rule for PrimOps like 'IntAddC' and 'WordAddC', where, in addition
-- to the result, we have to indicate that no carry/overflow occurred.
identityCPlatform :: (Platform -> Literal) -> RuleM CoreExpr
identityCPlatform lit =
  leftIdentityCPlatform lit `mplus` rightIdentityCPlatform lit

leftZero :: RuleM CoreExpr
leftZero = do
  [Lit l1, _] <- getArgs
  guard $ isZeroLit l1
  return $ Lit l1

rightZero :: RuleM CoreExpr
rightZero = do
  [_, Lit l2] <- getArgs
  guard $ isZeroLit l2
  return $ Lit l2

zeroElem :: RuleM CoreExpr
zeroElem = leftZero `mplus` rightZero

equalArgs :: RuleM ()
equalArgs = do
  [e1, e2] <- getArgs
  guard $ e1 `cheapEqExpr` e2

nonZeroLit :: Int -> RuleM ()
nonZeroLit n = getLiteral n >>= guard . not . isZeroLit

oneLit :: Int -> RuleM ()
oneLit n = getLiteral n >>= guard . isOneLit

-- When excess precision is not requested, cut down the precision of the
-- Rational value to that of Float/Double. We confuse host architecture
-- and target architecture here, but it's convenient (and wrong :-).
convFloating :: RuleOpts -> Literal -> Literal
convFloating env (LitFloat  f) | not (roExcessRationalPrecision env) =
   LitFloat  (toRational (fromRational f :: Float ))
convFloating env (LitDouble d) | not (roExcessRationalPrecision env) =
   LitDouble (toRational (fromRational d :: Double))
convFloating _ l = l

guardFloatDiv :: RuleM ()
guardFloatDiv = do
  [Lit (LitFloat f1), Lit (LitFloat f2)] <- getArgs
  guard $ (f1 /=0 || f2 > 0) -- see Note [negative zero]
       && f2 /= 0            -- avoid NaN and Infinity/-Infinity

guardDoubleDiv :: RuleM ()
guardDoubleDiv = do
  [Lit (LitDouble d1), Lit (LitDouble d2)] <- getArgs
  guard $ (d1 /=0 || d2 > 0) -- see Note [negative zero]
       && d2 /= 0            -- avoid NaN and Infinity/-Infinity
-- Note [negative zero] Avoid (0 / -d), otherwise 0/(-1) reduces to
-- zero, but we might want to preserve the negative zero here which
-- is representable in Float/Double but not in (normalised)
-- Rational. (#3676) Perhaps we should generate (0 :% (-1)) instead?

strengthReduction :: Literal -> PrimOp -> RuleM CoreExpr
strengthReduction two_lit add_op = do -- Note [Strength reduction]
  arg <- msum [ do [arg, Lit mult_lit] <- getArgs
                   guard (mult_lit == two_lit)
                   return arg
              , do [Lit mult_lit, arg] <- getArgs
                   guard (mult_lit == two_lit)
                   return arg ]
  return $ Var (mkPrimOpId add_op) `App` arg `App` arg

-- Note [Strength reduction]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- This rule turns floating point multiplications of the form 2.0 * x and
-- x * 2.0 into x + x addition, because addition costs less than multiplication.
-- See #7116

-- Note [What's true and false]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- trueValInt and falseValInt represent true and false values returned by
-- comparison primops for Char, Int, Word, Integer, Double, Float and Addr.
-- True is represented as an unboxed 1# literal, while false is represented
-- as 0# literal.
-- We still need Bool data constructors (True and False) to use in a rule
-- for constant folding of equal Strings

trueValInt, falseValInt :: Platform -> Expr CoreBndr
trueValInt  platform = Lit $ onei  platform -- see Note [What's true and false]
falseValInt platform = Lit $ zeroi platform

trueValBool, falseValBool :: Expr CoreBndr
trueValBool   = Var trueDataConId -- see Note [What's true and false]
falseValBool  = Var falseDataConId

ltVal, eqVal, gtVal :: Expr CoreBndr
ltVal = Var ordLTDataConId
eqVal = Var ordEQDataConId
gtVal = Var ordGTDataConId

mkIntVal :: Platform -> Integer -> Expr CoreBndr
mkIntVal platform i = Lit (mkLitInt platform i)
mkFloatVal :: RuleOpts -> Rational -> Expr CoreBndr
mkFloatVal env f = Lit (convFloating env (LitFloat  f))
mkDoubleVal :: RuleOpts -> Rational -> Expr CoreBndr
mkDoubleVal env d = Lit (convFloating env (LitDouble d))

matchPrimOpId :: PrimOp -> Id -> RuleM ()
matchPrimOpId op id = do
  op' <- liftMaybe $ isPrimOpId_maybe id
  guard $ op == op'

{-
************************************************************************
*                                                                      *
\subsection{Special rules for seq, tagToEnum, dataToTag}
*                                                                      *
************************************************************************

Note [tagToEnum#]
~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  Unification may refine the type later, but this
check won't see that, alas.  It's crude but it works.

Here's are two cases that should fail
        f :: forall a. a
        f = tagToEnum# 0        -- Can't do tagToEnum# at a type variable

        g :: Int
        g = tagToEnum# 0        -- Int is not an enumeration

We used to make this check in the type inference engine, but it's quite
ugly to do so, because the delayed constraint solving means that we don't
really know what's going on until the end. It's very much a corner case
because we don't expect the user to call tagToEnum# at all; we merely
generate calls in derived instances of Enum.  So we compromise: a
rewrite rule rewrites a bad instance of tagToEnum# to an error call,
and emits a warning.
-}

tagToEnumRule :: RuleM CoreExpr
-- If     data T a = A | B | C
-- then   tagToEnum# (T ty) 2# -->  B ty
tagToEnumRule = do
  [Type ty, Lit (LitNumber LitNumInt i)] <- getArgs
  case splitTyConApp_maybe ty of
    Just (tycon, tc_args) | isEnumerationTyCon tycon -> do
      let tag = fromInteger i
          correct_tag dc = (dataConTagZ dc) == tag
      (dc:rest) <- return $ filter correct_tag (tyConDataCons_maybe tycon `orElse` [])
      ASSERT(null rest) return ()
      return $ mkTyApps (Var (dataConWorkId dc)) tc_args

    -- See Note [tagToEnum#]
    _ -> WARN( True, text "tagToEnum# on non-enumeration type" <+> ppr ty )
         return $ mkRuntimeErrorApp rUNTIME_ERROR_ID ty "tagToEnum# on non-enumeration type"

------------------------------
dataToTagRule :: RuleM CoreExpr
-- See Note [dataToTag#] in primops.txt.pp
dataToTagRule = a `mplus` b
  where
    -- dataToTag (tagToEnum x)   ==>   x
    a = do
      [Type ty1, Var tag_to_enum `App` Type ty2 `App` tag] <- getArgs
      guard $ tag_to_enum `hasKey` tagToEnumKey
      guard $ ty1 `eqType` ty2
      return tag

    -- dataToTag (K e1 e2)  ==>   tag-of K
    -- This also works (via exprIsConApp_maybe) for
    --   dataToTag x
    -- where x's unfolding is a constructor application
    b = do
      dflags <- getPlatform
      [_, val_arg] <- getArgs
      in_scope <- getInScopeEnv
      (_,floats, dc,_,_) <- liftMaybe $ exprIsConApp_maybe in_scope val_arg
      ASSERT( not (isNewTyCon (dataConTyCon dc)) ) return ()
      return $ wrapFloats floats (mkIntVal dflags (toInteger (dataConTagZ dc)))

{- Note [dataToTag# magic]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The primop dataToTag# is unusual because it evaluates its argument.
Only `SeqOp` shares that property.  (Other primops do not do anything
as fancy as argument evaluation.)  The special handling for dataToTag#
is:

* GHC.Core.Utils.exprOkForSpeculation has a special case for DataToTagOp,
  (actually in app_ok).  Most primops with lifted arguments do not
  evaluate those arguments, but DataToTagOp and SeqOp are two
  exceptions.  We say that they are /never/ ok-for-speculation,
  regardless of the evaluated-ness of their argument.
  See GHC.Core.Utils Note [exprOkForSpeculation and SeqOp/DataToTagOp]

* There is a special case for DataToTagOp in GHC.StgToCmm.Expr.cgExpr,
  that evaluates its argument and then extracts the tag from
  the returned value.

* An application like (dataToTag# (Just x)) is optimised by
  dataToTagRule in GHC.Core.Opt.ConstantFold.

* A case expression like
     case (dataToTag# e) of <alts>
  gets transformed t
     case e of <transformed alts>
  by GHC.Core.Opt.ConstantFold.caseRules; see Note [caseRules for dataToTag]

See #15696 for a long saga.
-}

{- *********************************************************************
*                                                                      *
             unsafeEqualityProof
*                                                                      *
********************************************************************* -}

-- unsafeEqualityProof k t t  ==>  UnsafeRefl (Refl t)
-- That is, if the two types are equal, it's not unsafe!

unsafeEqualityProofRule :: RuleM CoreExpr
unsafeEqualityProofRule
  = do { [Type rep, Type t1, Type t2] <- getArgs
       ; guard (t1 `eqType` t2)
       ; fn <- getFunction
       ; let (_, ue) = splitForAllTyCoVars (idType fn)
             tc      = tyConAppTyCon ue  -- tycon:    UnsafeEquality
             (dc:_)  = tyConDataCons tc  -- data con: UnsafeRefl
             -- UnsafeRefl :: forall (r :: RuntimeRep) (a :: TYPE r).
             --               UnsafeEquality r a a
       ; return (mkTyApps (Var (dataConWrapId dc)) [rep, t1]) }


{- *********************************************************************
*                                                                      *
             Rules for seq# and spark#
*                                                                      *
********************************************************************* -}

{- Note [seq# magic]
~~~~~~~~~~~~~~~~~~~~
The primop
   seq# :: forall a s . a -> State# s -> (# State# s, a #)

is /not/ the same as the Prelude function seq :: a -> b -> b
as you can see from its type.  In fact, seq# is the implementation
mechanism for 'evaluate'

   evaluate :: a -> IO a
   evaluate a = IO $ \s -> seq# a s

The semantics of seq# is
  * evaluate its first argument
  * and return it

Things to note

* Why do we need a primop at all?  That is, instead of
      case seq# x s of (# x, s #) -> blah
  why not instead say this?
      case x of { DEFAULT -> blah)

  Reason (see #5129): if we saw
    catch# (\s -> case x of { DEFAULT -> raiseIO# exn s }) handler

  then we'd drop the 'case x' because the body of the case is bottom
  anyway. But we don't want to do that; the whole /point/ of
  seq#/evaluate is to evaluate 'x' first in the IO monad.

  In short, we /always/ evaluate the first argument and never
  just discard it.

* Why return the value?  So that we can control sharing of seq'd
  values: in
     let x = e in x `seq` ... x ...
  We don't want to inline x, so better to represent it as
       let x = e in case seq# x RW of (# _, x' #) -> ... x' ...
  also it matches the type of rseq in the Eval monad.

Implementing seq#.  The compiler has magic for SeqOp in

- GHC.Core.Opt.ConstantFold.seqRule: eliminate (seq# <whnf> s)

- GHC.StgToCmm.Expr.cgExpr, and cgCase: special case for seq#

- GHC.Core.Utils.exprOkForSpeculation;
  see Note [exprOkForSpeculation and SeqOp/DataToTagOp] in GHC.Core.Utils

- Simplify.addEvals records evaluated-ness for the result; see
  Note [Adding evaluatedness info to pattern-bound variables]
  in GHC.Core.Opt.Simplify
-}

seqRule :: RuleM CoreExpr
seqRule = do
  [Type ty_a, Type _ty_s, a, s] <- getArgs
  guard $ exprIsHNF a
  return $ mkCoreUbxTup [exprType s, ty_a] [s, a]

-- spark# :: forall a s . a -> State# s -> (# State# s, a #)
sparkRule :: RuleM CoreExpr
sparkRule = seqRule -- reduce on HNF, just the same
  -- XXX perhaps we shouldn't do this, because a spark eliminated by
  -- this rule won't be counted as a dud at runtime?

{-
************************************************************************
*                                                                      *
\subsection{Built in rules}
*                                                                      *
************************************************************************

Note [Scoping for Builtin rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When compiling a (base-package) module that defines one of the
functions mentioned in the RHS of a built-in rule, there's a danger
that we'll see

        f = ...(eq String x)....

        ....and lower down...

        eqString = ...

Then a rewrite would give

        f = ...(eqString x)...
        ....and lower down...
        eqString = ...

and lo, eqString is not in scope.  This only really matters when we
get to code generation.  But the occurrence analyser does a GlomBinds
step when necessary, that does a new SCC analysis on the whole set of
bindings (see occurAnalysePgm), which sorts out the dependency, so all
is fine.
-}

builtinRules :: [CoreRule]
-- Rules for non-primops that can't be expressed using a RULE pragma
builtinRules
  = [BuiltinRule { ru_name = fsLit "AppendLitString",
                   ru_fn = unpackCStringFoldrName,
                   ru_nargs = 4, ru_try = match_append_lit_C },
     BuiltinRule { ru_name = fsLit "AppendLitStringUtf8",
                   ru_fn = unpackCStringFoldrUtf8Name,
                   ru_nargs = 4, ru_try = match_append_lit_utf8 },
     BuiltinRule { ru_name = fsLit "EqString", ru_fn = eqStringName,
                   ru_nargs = 2, ru_try = match_eq_string },
     BuiltinRule { ru_name = fsLit "CStringLength", ru_fn = cstringLengthName,
                   ru_nargs = 1, ru_try = match_cstring_length },
     BuiltinRule { ru_name = fsLit "Inline", ru_fn = inlineIdName,
                   ru_nargs = 2, ru_try = \_ _ _ -> match_inline },
     BuiltinRule { ru_name = fsLit "MagicDict", ru_fn = idName magicDictId,
                   ru_nargs = 4, ru_try = \_ _ _ -> match_magicDict },

     mkBasicRule unsafeEqualityProofName 3 unsafeEqualityProofRule,

     mkBasicRule divIntName 2 $ msum
        [ nonZeroLit 1 >> binaryLit (intOp2 div)
        , leftZero
        , do
          [arg, Lit (LitNumber LitNumInt d)] <- getArgs
          Just n <- return $ exactLog2 d
          platform <- getPlatform
          return $ Var (mkPrimOpId IntSraOp) `App` arg `App` mkIntVal platform n
        ],

     mkBasicRule modIntName 2 $ msum
        [ nonZeroLit 1 >> binaryLit (intOp2 mod)
        , leftZero
        , do
          [arg, Lit (LitNumber LitNumInt d)] <- getArgs
          Just _ <- return $ exactLog2 d
          platform <- getPlatform
          return $ Var (mkPrimOpId IntAndOp)
            `App` arg `App` mkIntVal platform (d - 1)
        ]
     ]
 ++ builtinBignumRules
{-# NOINLINE builtinRules #-}
-- there is no benefit to inlining these yet, despite this, GHC produces
-- unfoldings for this regardless since the floated list entries look small.

builtinBignumRules :: [CoreRule]
builtinBignumRules =
  [ -- conversions
    lit_to_integer "Word# -> Integer"   integerFromWordName
  , lit_to_integer "Int64# -> Integer"  integerFromInt64Name
  , lit_to_integer "Word64# -> Integer" integerFromWord64Name
  , lit_to_integer "Natural -> Integer" integerFromNaturalName

  , integer_to_lit "Integer -> Word# (wrap)"   integerToWordName   mkWordLitWrap
  , integer_to_lit "Integer -> Int# (wrap)"    integerToIntName    mkIntLitWrap
  , integer_to_lit "Integer -> Word64# (wrap)" integerToWord64Name (\_ -> mkWord64LitWord64 . fromInteger)
  , integer_to_lit "Integer -> Int64# (wrap)"  integerToInt64Name  (\_ -> mkInt64LitInt64 . fromInteger)
  , integer_to_lit "Integer -> Float#"         integerToFloatName  (\_ -> mkFloatLitFloat . fromInteger)
  , integer_to_lit "Integer -> Double#"        integerToDoubleName (\_ -> mkDoubleLitDouble . fromInteger)

  , integer_to_natural "Integer -> Natural (clamp)" integerToNaturalClampName False True
  , integer_to_natural "Integer -> Natural (wrap)"  integerToNaturalName      False False
  , integer_to_natural "Integer -> Natural (throw)" integerToNaturalThrowName True False

  , lit_to_natural  "Word# -> Natural"         naturalNSName
  , natural_to_word "Natural -> Word# (wrap)"  naturalToWordName      False
  , natural_to_word "Natural -> Word# (clamp)" naturalToWordClampName True

    -- comparisons (return an unlifted Int#)
  , integer_cmp "integerEq#" integerEqName (==)
  , integer_cmp "integerNe#" integerNeName (/=)
  , integer_cmp "integerLe#" integerLeName (<=)
  , integer_cmp "integerGt#" integerGtName (>)
  , integer_cmp "integerLt#" integerLtName (<)
  , integer_cmp "integerGe#" integerGeName (>=)

  , natural_cmp "naturalEq#" naturalEqName (==)
  , natural_cmp "naturalNe#" naturalNeName (/=)
  , natural_cmp "naturalLe#" naturalLeName (<=)
  , natural_cmp "naturalGt#" naturalGtName (>)
  , natural_cmp "naturalLt#" naturalLtName (<)
  , natural_cmp "naturalGe#" naturalGeName (>=)

    -- comparisons (return an Ordering)
  , bignum_compare "integerCompare" integerCompareName
  , bignum_compare "naturalCompare" naturalCompareName

    -- binary operations
  , integer_binop "integerAdd" integerAddName (+)
  , integer_binop "integerSub" integerSubName (-)
  , integer_binop "integerMul" integerMulName (*)
  , integer_binop "integerGcd" integerGcdName gcd
  , integer_binop "integerLcm" integerLcmName lcm
  , integer_binop "integerAnd" integerAndName (.&.)
  , integer_binop "integerOr"  integerOrName  (.|.)
  , integer_binop "integerXor" integerXorName xor

  , natural_binop "naturalAdd" naturalAddName (+)
  , natural_binop "naturalMul" naturalMulName (*)
  , natural_binop "naturalGcd" naturalGcdName gcd
  , natural_binop "naturalLcm" naturalLcmName lcm
  , natural_binop "naturalAnd" naturalAndName (.&.)
  , natural_binop "naturalOr"  naturalOrName  (.|.)
  , natural_binop "naturalXor" naturalXorName xor

    -- Natural subtraction: it's a binop but it can fail because of underflow so
    -- we have several primitives to handle here.
  , natural_sub "naturalSubUnsafe" naturalSubUnsafeName
  , natural_sub "naturalSubThrow"  naturalSubThrowName
  , mkRule "naturalSub" naturalSubName 2 $ do
        [a0,a1] <- getArgs
        x <- isNaturalLiteral a0
        y <- isNaturalLiteral a1
        -- return an unboxed sum: (# (# #) | Natural #)
        let ret n v = pure $ mkCoreUbxSum 2 n [unboxedUnitTy,naturalTy] v
        if x < y
            then ret 1 $ Var voidPrimId
            else ret 2 $ Lit (mkLitNatural (x - y))

    -- unary operations
  , bignum_unop "integerNegate"     integerNegateName     mkLitInteger negate
  , bignum_unop "integerAbs"        integerAbsName        mkLitInteger abs
  , bignum_unop "integerSignum"     integerSignumName     mkLitInteger signum
  , bignum_unop "integerComplement" integerComplementName mkLitInteger complement

  , bignum_unop "naturalSignum"     naturalSignumName     mkLitNatural signum

  , mkRule "naturalNegate" naturalNegateName 1 $ do
        [a0] <- getArgs
        x <- isNaturalLiteral a0
        guard (x == 0) -- negate is only valid for (0 :: Natural)
        pure a0

  , bignum_popcount "integerPopCount" integerPopCountName mkLitIntWrap
  , bignum_popcount "naturalPopCount" naturalPopCountName mkLitWordWrap

  ------------------------------------------------------------
  -- The following `small_passthough_*` rules are used to optimise conversions
  -- between numeric types by avoiding passing through "small" constructors of
  -- Integer and Natural.
  --
  -- See Note [Optimising conversions between numeric types]
  --

  , small_passthrough_id "Word# -> Natural -> Word#"
      naturalNSName naturalToWordName
  , small_passthrough_id "Word# -> Natural -> Word# (clamp)"
      naturalNSName naturalToWordClampName

  , small_passthrough_id "Int# -> Integer -> Int#"
      integerISName integerToIntName
  , small_passthrough_id "Word# -> Integer -> Word#"
      integerFromWordName integerToWordName
  , small_passthrough_id "Int64# -> Integer -> Int64#"
      integerFromInt64Name integerToInt64Name
  , small_passthrough_id "Word64# -> Integer -> Word64#"
      integerFromWord64Name integerToWord64Name
  , small_passthrough_id "Natural -> Integer -> Natural (wrap)"
      integerFromNaturalName integerToNaturalName
  , small_passthrough_id "Natural -> Integer -> Natural (throw)"
      integerFromNaturalName integerToNaturalThrowName
  , small_passthrough_id "Natural -> Integer -> Natural (clamp)"
      integerFromNaturalName integerToNaturalClampName

  , small_passthrough_app "Int# -> Integer -> Word#"
        integerISName integerToWordName   (mkPrimOpId IntToWordOp)
  , small_passthrough_app "Int# -> Integer -> Float#"
        integerISName integerToFloatName  (mkPrimOpId IntToFloatOp)
  , small_passthrough_app "Int# -> Integer -> Double#"
        integerISName integerToDoubleName (mkPrimOpId IntToDoubleOp)

  , small_passthrough_app "Word# -> Integer -> Int#"
        integerFromWordName integerToIntName (mkPrimOpId WordToIntOp)
  , small_passthrough_app "Word# -> Integer -> Float#"
        integerFromWordName integerToFloatName (mkPrimOpId WordToFloatOp)
  , small_passthrough_app "Word# -> Integer -> Double#"
        integerFromWordName integerToDoubleName (mkPrimOpId WordToDoubleOp)
  , small_passthrough_app "Word# -> Integer -> Natural (wrap)"
        integerFromWordName integerToNaturalName naturalNSId
  , small_passthrough_app "Word# -> Integer -> Natural (throw)"
        integerFromWordName integerToNaturalThrowName naturalNSId
  , small_passthrough_app "Word# -> Integer -> Natural (clamp)"
        integerFromWordName integerToNaturalClampName naturalNSId

  , small_passthrough_app "Word# -> Natural -> Float#"
        naturalNSName naturalToFloatName  (mkPrimOpId WordToFloatOp)
  , small_passthrough_app "Word# -> Natural -> Double#"
        naturalNSName naturalToDoubleName (mkPrimOpId WordToDoubleOp)

#if WORD_SIZE_IN_BITS < 64
  , small_passthrough_id "Int64# -> Integer -> Int64#"
      integerFromInt64Name integerToInt64Name
  , small_passthrough_id "Word64# -> Integer -> Word64#"
      integerFromWord64Name integerToWord64Name

  , small_passthrough_app "Int64# -> Integer -> Word64#"
        integerFromInt64Name integerToWord64Name   (mkPrimOpId Int64ToWord64Op)
  , small_passthrough_app "Word64# -> Integer -> Int64#"
        integerFromWord64Name integerToInt64Name   (mkPrimOpId Word64ToInt64Op)

  , small_passthrough_app "Word# -> Integer -> Word64#"
        integerFromWordName integerToWord64Name (mkPrimOpId WordToWord64Op)
  , small_passthrough_app "Word64# -> Integer -> Word#"
        integerFromWord64Name integerToWordName (mkPrimOpId Word64ToWordOp)
  , small_passthrough_app "Int# -> Integer -> Int64#"
        integerISName integerToInt64Name (mkPrimOpId IntToInt64Op)
  , small_passthrough_app "Int64# -> Integer -> Int#"
        integerFromInt64Name integerToIntName (mkPrimOpId Int64ToIntOp)

  , small_passthrough_custom "Int# -> Integer -> Word64#"
        integerISName integerToWord64Name
          (\x -> Var (mkPrimOpId Int64ToWord64Op) `App` (Var (mkPrimOpId IntToInt64Op) `App` x))
  , small_passthrough_custom "Word64# -> Integer -> Int#"
        integerFromWord64Name integerToIntName
          (\x -> Var (mkPrimOpId WordToIntOp) `App` (Var (mkPrimOpId Word64ToWordOp) `App` x))
  , small_passthrough_custom "Word# -> Integer -> Int64#"
        integerFromWordName integerToInt64Name
          (\x -> Var (mkPrimOpId Word64ToInt64Op) `App` (Var (mkPrimOpId WordToWord64Op) `App` x))
  , small_passthrough_custom "Int64# -> Integer -> Word#"
        integerFromInt64Name integerToWordName
          (\x -> Var (mkPrimOpId IntToWordOp) `App` (Var (mkPrimOpId Int64ToIntOp) `App` x))
#endif

    -- Bits.bit
  , bignum_bit "integerBit" integerBitName mkLitInteger
  , bignum_bit "naturalBit" naturalBitName mkLitNatural

    -- Bits.testBit
  , bignum_testbit "integerTestBit" integerTestBitName
  , bignum_testbit "naturalTestBit" naturalTestBitName

    -- Bits.shift
  , bignum_shift "integerShiftL" integerShiftLName shiftL mkLitInteger
  , bignum_shift "integerShiftR" integerShiftRName shiftR mkLitInteger
  , bignum_shift "naturalShiftL" naturalShiftLName shiftL mkLitNatural
  , bignum_shift "naturalShiftR" naturalShiftRName shiftR mkLitNatural

    -- division
  , divop_one  "integerQuot"    integerQuotName    quot    mkLitInteger
  , divop_one  "integerRem"     integerRemName     rem     mkLitInteger
  , divop_one  "integerDiv"     integerDivName     div     mkLitInteger
  , divop_one  "integerMod"     integerModName     mod     mkLitInteger
  , divop_both "integerDivMod"  integerDivModName  divMod  mkLitInteger integerTy
  , divop_both "integerQuotRem" integerQuotRemName quotRem mkLitInteger integerTy

  , divop_one  "naturalQuot"    naturalQuotName    quot    mkLitNatural
  , divop_one  "naturalRem"     naturalRemName     rem     mkLitNatural
  , divop_both "naturalQuotRem" naturalQuotRemName quotRem mkLitNatural naturalTy

    -- conversions from Rational for Float/Double literals
  , rational_to "rationalToFloat"  rationalToFloatName  mkFloatExpr
  , rational_to "rationalToDouble" rationalToDoubleName mkDoubleExpr

    -- conversions from Integer for Float/Double literals
  , integer_encode_float "integerEncodeFloat"  integerEncodeFloatName  mkFloatLitFloat
  , integer_encode_float "integerEncodeDouble" integerEncodeDoubleName mkDoubleLitDouble
  ]
  where
    -- The rule is matching against an occurrence of a data constructor in a
    -- Core expression. It must match either its worker name or its wrapper
    -- name, /not/ the DataCon name itself, which is different.
    -- See Note [Data Constructor Naming] in GHC.Core.DataCon and #19892
    --
    -- But data constructor wrappers deliberately inline late; See Note
    -- [Activation for data constructor wrappers] in GHC.Types.Id.Make.
    -- Suppose there is a wrapper and the rule matches on the worker: the
    -- wrapper won't be inlined until rules have finished firing and the rule
    -- will never fire.
    --
    -- Hence the rule must match on the wrapper, if there is one, otherwise on
    -- the worker. That is exactly the dataConWrapId for the data constructor.
    -- The data constructor may or may not have a wrapper, but if not
    -- dataConWrapId will return the worker
    --
    integerISId   = dataConWrapId integerISDataCon
    naturalNSId   = dataConWrapId naturalNSDataCon
    integerISName = idName integerISId
    naturalNSName = idName naturalNSId

    mkRule str name nargs f = BuiltinRule
      { ru_name = fsLit str
      , ru_fn = name
      , ru_nargs = nargs
      , ru_try = runRuleM $ do
          env <- getRuleOpts
          guard (roBignumRules env)
          f
      }

    integer_to_lit str name convert = mkRule str name 1 $ do
      [a0] <- getArgs
      platform <- getPlatform
      x <- isIntegerLiteral a0
      pure (convert platform x)

    natural_to_word str name clamp = mkRule str name 1 $ do
      [a0] <- getArgs
      n <- isNaturalLiteral a0
      platform <- getPlatform
      if clamp && not (platformInWordRange platform n)
          then pure (Lit (mkLitWord platform (platformMaxWord platform)))
          else pure (Lit (mkLitWordWrap platform n))

    integer_to_natural str name thrw clamp = mkRule str name 1 $ do
      [a0] <- getArgs
      x <- isIntegerLiteral a0
      if | x >= 0    -> pure $ Lit $ mkLitNatural x
         | thrw      -> mzero
         | clamp     -> pure $ Lit $ mkLitNatural 0       -- clamp to 0
         | otherwise -> pure $ Lit $ mkLitNatural (abs x) -- negate/wrap

    lit_to_integer str name = mkRule str name 1 $ do
      [a0] <- getArgs
      isLiteral a0 >>= \case
        -- convert any numeric literal into an Integer literal
        LitNumber _ i -> pure (Lit (mkLitInteger i))
        _             -> mzero

    lit_to_natural str name = mkRule str name 1 $ do
      [a0] <- getArgs
      isLiteral a0 >>= \case
        -- convert any *positive* numeric literal into a Natural literal
        LitNumber _ i | i >= 0 -> pure (Lit (mkLitNatural i))
        _                      -> mzero

    integer_binop str name op = mkRule str name 2 $ do
      [a0,a1] <- getArgs
      x <- isIntegerLiteral a0
      y <- isIntegerLiteral a1
      pure (Lit (mkLitInteger (x `op` y)))

    natural_binop str name op = mkRule str name 2 $ do
      [a0,a1] <- getArgs
      x <- isNaturalLiteral a0
      y <- isNaturalLiteral a1
      pure (Lit (mkLitNatural (x `op` y)))

    natural_sub str name = mkRule str name 2 $ do
      [a0,a1] <- getArgs
      x <- isNaturalLiteral a0
      y <- isNaturalLiteral a1
      guard (x >= y)
      pure (Lit (mkLitNatural (x - y)))

    integer_cmp str name op = mkRule str name 2 $ do
      platform <- getPlatform
      [a0,a1] <- getArgs
      x <- isIntegerLiteral a0
      y <- isIntegerLiteral a1
      pure $ if x `op` y
              then trueValInt platform
              else falseValInt platform

    natural_cmp str name op = mkRule str name 2 $ do
      platform <- getPlatform
      [a0,a1] <- getArgs
      x <- isNaturalLiteral a0
      y <- isNaturalLiteral a1
      pure $ if x `op` y
              then trueValInt platform
              else falseValInt platform

    bignum_compare str name = mkRule str name 2 $ do
      [a0,a1] <- getArgs
      x <- isNumberLiteral a0
      y <- isNumberLiteral a1
      pure $ case x `compare` y of
              LT -> ltVal
              EQ -> eqVal
              GT -> gtVal

    bignum_unop str name mk_lit op = mkRule str name 1 $ do
      [a0] <- getArgs
      x <- isNumberLiteral a0
      pure $ Lit (mk_lit (op x))

    bignum_popcount str name mk_lit = mkRule str name 1 $ do
      platform <- getPlatform
      -- We use a host Int to compute the popCount. If we compile on a 32-bit
      -- host for a 64-bit target, the result may be different than if computed
      -- by the target. So we disable this rule if sizes don't match.
      guard (platformWordSizeInBits platform == finiteBitSize (0 :: Word))
      [a0] <- getArgs
      x <- isNumberLiteral a0
      pure $ Lit (mk_lit platform (fromIntegral (popCount x)))

    small_passthrough_id str from_x to_x =
      small_passthrough_custom str from_x to_x id

    small_passthrough_app str from_x to_y x_to_y =
      small_passthrough_custom str from_x to_y (App (Var x_to_y))

    small_passthrough_custom str from_x to_y x_to_y = mkRule str to_y 1 $ do
      [a0] <- getArgs
      env <- getEnv
      (f,x) <- isVarApp env a0
      guard (idName f == from_x)
      pure $ x_to_y x

    bignum_bit str name mk_lit = mkRule str name 1 $ do
      [a0] <- getArgs
      platform <- getPlatform
      n <- isNumberLiteral a0
      -- Make sure n is positive and small enough to yield a decently
      -- small number. Attempting to construct the Integer for
      --    (integerBit 9223372036854775807#)
      -- would be a bad idea (#14959)
      guard (n >= 0 && n <= fromIntegral (platformWordSizeInBits platform))
      -- it's safe to convert a target Int value into a host Int value
      -- to perform the "bit" operation because n is very small (<= 64).
      pure $ Lit (mk_lit (bit (fromIntegral n)))

    bignum_testbit str name = mkRule str name 2 $ do
      [a0,a1] <- getArgs
      platform <- getPlatform
      x <- isNumberLiteral a0
      n <- isNumberLiteral a1
      -- ensure that we can store 'n' in a host Int
      guard (n >= 0 && n <= fromIntegral (maxBound :: Int))
      pure $ if testBit x (fromIntegral n)
              then trueValInt platform
              else falseValInt platform

    bignum_shift str name shift_op mk_lit = mkRule str name 2 $ do
      [a0,a1] <- getArgs
      x <- isNumberLiteral a0
      n <- isWordLiteral a1
      -- See Note [Guarding against silly shifts]
      -- Restrict constant-folding of shifts on Integers, somewhat arbitrary.
      -- We can get huge shifts in inaccessible code (#15673)
      guard (n <= 4)
      pure $ Lit (mk_lit (x `shift_op` fromIntegral n))

    divop_one str name divop mk_lit = mkRule str name 2 $ do
      [a0,a1] <- getArgs
      n <- isNumberLiteral a0
      d <- isNumberLiteral a1
      guard (d /= 0)
      pure $ Lit (mk_lit (n `divop` d))

    divop_both str name divop mk_lit ty = mkRule str name 2 $ do
      [a0,a1] <- getArgs
      n <- isNumberLiteral a0
      d <- isNumberLiteral a1
      guard (d /= 0)
      let (r,s) = n `divop` d
      pure $ mkCoreUbxTup [ty,ty] [Lit (mk_lit r), Lit (mk_lit s)]

    integer_encode_float :: RealFloat a => String -> Name -> (a -> CoreExpr) -> CoreRule
    integer_encode_float str name mk_lit = mkRule str name 2 $ do
      [a0,a1] <- getArgs
      x <- isIntegerLiteral a0
      y <- isIntLiteral a1
      -- check that y (a target Int) is in the host Int range
      guard (y <= fromIntegral (maxBound :: Int))
      pure (mk_lit $ encodeFloat x (fromInteger y))

    rational_to :: RealFloat a => String -> Name -> (a -> CoreExpr) -> CoreRule
    rational_to str name mk_lit = mkRule str name 2 $ do
      -- This turns `rationalToFloat n d` where `n` and `d` are literals into
      -- a literal Float (and similarly for Double).
      [a0,a1] <- getArgs
      n <- isIntegerLiteral a0
      d <- isIntegerLiteral a1
      -- it's important to not match d == 0, because that may represent a
      -- literal "0/0" or similar, and we can't produce a literal value for
      -- NaN or +-Inf
      guard (d /= 0)
      pure $ mk_lit (fromRational (n % d))



---------------------------------------------------
-- The rule is this:
--      unpackFoldrCString*# "foo"# c (unpackFoldrCString*# "baz"# c n)
--      =  unpackFoldrCString*# "foobaz"# c n
--
-- See also Note [String literals in GHC] in CString.hs

-- CString version
match_append_lit_C :: RuleFun
match_append_lit_C = match_append_lit unpackCStringFoldrIdKey

-- CStringUTF8 version
match_append_lit_utf8 :: RuleFun
match_append_lit_utf8 = match_append_lit unpackCStringFoldrUtf8IdKey

{-# INLINE match_append_lit #-}
match_append_lit :: Unique -> RuleFun
match_append_lit foldVariant _ id_unf _
        [ Type ty1
        , lit1
        , c1
        , e2
        ]
  -- N.B. Ensure that we strip off any ticks (e.g. source notes) from the
  -- `lit` and `c` arguments, lest this may fail to fire when building with
  -- -g3. See #16740.
  | (strTicks, Var unpk `App` Type ty2
                        `App` lit2
                        `App` c2
                        `App` n) <- stripTicksTop tickishFloatable e2
  , unpk `hasKey` foldVariant
  , Just (LitString s1) <- exprIsLiteral_maybe id_unf lit1
  , Just (LitString s2) <- exprIsLiteral_maybe id_unf lit2
  , let freeVars = (mkInScopeSet (exprFreeVars c1 `unionVarSet` exprFreeVars c2))
    in eqExpr freeVars c1 c2
  , (c1Ticks, c1') <- stripTicksTop tickishFloatable c1
  , c2Ticks <- stripTicksTopT tickishFloatable c2
  = ASSERT( ty1 `eqType` ty2 )
    Just $ mkTicks strTicks
         $ Var unpk `App` Type ty1
                    `App` Lit (LitString (s1 `BS.append` s2))
                    `App` mkTicks (c1Ticks ++ c2Ticks) c1'
                    `App` n

match_append_lit _ _ _ _ _ = Nothing

---------------------------------------------------
-- The rule is this:
--      eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2)) = s1==s2
-- Also  matches unpackCStringUtf8#

match_eq_string :: RuleFun
match_eq_string _ id_unf _
        [Var unpk1 `App` lit1, Var unpk2 `App` lit2]
  | unpk_key1 <- getUnique unpk1
  , unpk_key2 <- getUnique unpk2
  , unpk_key1 == unpk_key2
  -- For now we insist the literals have to agree in their encoding
  -- to keep the rule simple. But we could check if the decoded strings
  -- compare equal in here as well.
  , unpk_key1 `elem` [unpackCStringUtf8IdKey, unpackCStringIdKey]
  , Just (LitString s1) <- exprIsLiteral_maybe id_unf lit1
  , Just (LitString s2) <- exprIsLiteral_maybe id_unf lit2
  = Just (if s1 == s2 then trueValBool else falseValBool)

match_eq_string _ _ _ _ = Nothing

-----------------------------------------------------------------------
-- Illustration of this rule:
--
-- cstringLength# "foobar"# --> 6
-- cstringLength# "fizz\NULzz"# --> 4
--
-- Nota bene: Addr# literals are suffixed by a NUL byte when they are
-- compiled to read-only data sections. That's why cstringLength# is
-- well defined on Addr# literals that do not explicitly have an embedded
-- NUL byte.
--
-- See GHC issue #5218, MR 2165, and bytestring PR 191. This is particularly
-- helpful when using OverloadedStrings to create a ByteString since the
-- function computing the length of such ByteStrings can often be constant
-- folded.
match_cstring_length :: RuleFun
match_cstring_length env id_unf _ [lit1]
  | Just (LitString str) <- exprIsLiteral_maybe id_unf lit1
    -- If elemIndex returns Just, it has the index of the first embedded NUL
    -- in the string. If no NUL bytes are present (the common case) then use
    -- full length of the byte string.
  = let len = fromMaybe (BS.length str) (BS.elemIndex 0 str)
     in Just (Lit (mkLitInt (roPlatform env) (fromIntegral len)))
match_cstring_length _ _ _ _ = Nothing

---------------------------------------------------
{- Note [inlineId magic]
~~~~~~~~~~~~~~~~~~~~~~~~
The call 'inline f' arranges that 'f' is inlined, regardless of
its size. More precisely, the call 'inline f' rewrites to the
right-hand side of 'f's definition. This allows the programmer to
control inlining from a particular call site rather than the
definition site of the function.

The moving parts are simple:

* A very simple definition in the library base:GHC.Magic
     {-# NOINLINE[0] inline #-}
     inline :: a -> a
     inline x = x
  So in phase 0, 'inline' will be inlined, so its use imposes
  no overhead.

* A rewrite rule, in GHC.Core.Opt.ConstantFold, which makes
  (inline f) inline, implemented by match_inline.
  The rule for the 'inline' function is this:
     inline f_ty (f a b c) = <f's unfolding> a b c
  (if f has an unfolding, EVEN if it's a loop breaker)

  It's important to allow the argument to 'inline' to have args itself
  (a) because its more forgiving to allow the programmer to write
      either  inline f a b c
      or      inline (f a b c)
  (b) because a polymorphic f wll get a type argument that the
      programmer can't avoid, so the call may look like
        inline (map @Int @Bool) g xs

  Also, don't forget about 'inline's type argument!
-}

match_inline :: [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_inline (Type _ : e : _)
  | (Var f, args1) <- collectArgs e,
    Just unf <- maybeUnfoldingTemplate (realIdUnfolding f)
             -- Ignore the IdUnfoldingFun here!
  = Just (mkApps unf args1)

match_inline _ = Nothing

---------------------------------------------------
-- See Note [magicDictId magic] in "GHC.Types.Id.Make"
-- for a description of what is going on here.
match_magicDict :: [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_magicDict [Type _, (stripTicksE (const True) -> (Var wrap `App` Type a `App` Type _ `App` f)), x, y ]
  | Just (_, fieldTy, _)  <- splitFunTy_maybe $ dropForAlls $ idType wrap
  , Just (_, dictTy, _)   <- splitFunTy_maybe fieldTy
  , Just dictTc           <- tyConAppTyCon_maybe dictTy
  , Just (_,_,co)         <- unwrapNewTyCon_maybe dictTc
  = Just
  $ f `App` Cast x (mkSymCo (mkUnbranchedAxInstCo Representational co [a] []))
      `App` y

match_magicDict _ = Nothing

--------------------------------------------------------
-- Note [Constant folding through nested expressions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We use rewrites rules to perform constant folding. It means that we don't
-- have a global view of the expression we are trying to optimise. As a
-- consequence we only perform local (small-step) transformations that either:
--    1) reduce the number of operations
--    2) rearrange the expression to increase the odds that other rules will
--    match
--
-- We don't try to handle more complex expression optimisation cases that would
-- require a global view. For example, rewriting expressions to increase
-- sharing (e.g., Horner's method); optimisations that require local
-- transformations increasing the number of operations; rearrangements to
-- cancel/factorize terms (e.g., (a+b-a-b) isn't rearranged to reduce to 0).
--
-- We already have rules to perform constant folding on expressions with the
-- following shape (where a and/or b are literals):
--
--          D)    op
--                /\
--               /  \
--              /    \
--             a      b
--
-- To support nested expressions, we match three other shapes of expression
-- trees:
--
-- A)   op1          B)       op1       C)       op1
--      /\                    /\                 /\
--     /  \                  /  \               /  \
--    /    \                /    \             /    \
--   a     op2            op2     c          op2    op3
--          /\            /\                 /\      /\
--         /  \          /  \               /  \    /  \
--        b    c        a    b             a    b  c    d
--
--
-- R1) +/- simplification:
--    ops = + or -, two literals (not siblings)
--
--    Examples:
--       A: 5 + (10-x)  ==> 15-x
--       B: (10+x) + 5  ==> 15+x
--       C: (5+a)-(5-b) ==> 0+(a+b)
--
-- R2) * simplification
--    ops = *, two literals (not siblings)
--
--    Examples:
--       A: 5 * (10*x)  ==> 50*x
--       B: (10*x) * 5  ==> 50*x
--       C: (5*a)*(5*b) ==> 25*(a*b)
--
-- R3) * distribution over +/-
--    op1 = *, op2 = + or -, two literals (not siblings)
--
--    This transformation doesn't reduce the number of operations but switches
--    the outer and the inner operations so that the outer is (+) or (-) instead
--    of (*). It increases the odds that other rules will match after this one.
--
--    Examples:
--       A: 5 * (10-x)  ==> 50 - (5*x)
--       B: (10+x) * 5  ==> 50 + (5*x)
--       C: Not supported as it would increase the number of operations:
--          (5+a)*(5-b) ==> 25 - 5*b + 5*a - a*b
--
-- R4) Simple factorization
--
--    op1 = + or -, op2/op3 = *,
--    one literal for each innermost * operation (except in the D case),
--    the two other terms are equals
--
--    Examples:
--       A: x - (10*x)  ==> (-9)*x
--       B: (10*x) + x  ==> 11*x
--       C: (5*x)-(x*3) ==> 2*x
--       D: x+x         ==> 2*x
--
-- R5) +/- propagation
--
--    ops = + or -, one literal
--
--    This transformation doesn't reduce the number of operations but propagates
--    the constant to the outer level. It increases the odds that other rules
--    will match after this one.
--
--    Examples:
--       A: x - (10-y)  ==> (x+y) - 10
--       B: (10+x) - y  ==> 10 + (x-y)
--       C: N/A (caught by the A and B cases)
--
--------------------------------------------------------

-- Rules to perform constant folding into nested expressions
--
--See Note [Constant folding through nested expressions]

addFoldingRules :: PrimOp -> NumOps -> RuleM CoreExpr
addFoldingRules op num_ops = do
   ASSERT(op == numAdd num_ops) return ()
   env <- getRuleOpts
   guard (roNumConstantFolding env)
   [arg1,arg2] <- getArgs
   platform <- getPlatform
   liftMaybe
      -- commutativity for + is handled here
      (addFoldingRules' platform arg1 arg2 num_ops
       <|> addFoldingRules' platform arg2 arg1 num_ops)

subFoldingRules :: PrimOp -> NumOps -> RuleM CoreExpr
subFoldingRules op num_ops = do
   ASSERT(op == numSub num_ops) return ()
   env <- getRuleOpts
   guard (roNumConstantFolding env)
   [arg1,arg2] <- getArgs
   platform <- getPlatform
   liftMaybe (subFoldingRules' platform arg1 arg2 num_ops)

mulFoldingRules :: PrimOp -> NumOps -> RuleM CoreExpr
mulFoldingRules op num_ops = do
   ASSERT(op == numMul num_ops) return ()
   env <- getRuleOpts
   guard (roNumConstantFolding env)
   [arg1,arg2] <- getArgs
   platform <- getPlatform
   liftMaybe
      -- commutativity for * is handled here
      (mulFoldingRules' platform arg1 arg2 num_ops
       <|> mulFoldingRules' platform arg2 arg1 num_ops)


addFoldingRules' :: Platform -> CoreExpr -> CoreExpr -> NumOps -> Maybe CoreExpr
addFoldingRules' platform arg1 arg2 num_ops = case (arg1, arg2) of
      -- R1) +/- simplification

      -- l1 + (l2 + x) ==> (l1+l2) + x
      (L l1, is_lit_add num_ops -> Just (l2,x))
         -> Just (mkL (l1+l2) `add` x)

      -- l1 + (l2 - x) ==> (l1+l2) - x
      (L l1, is_sub num_ops -> Just (L l2,x))
         -> Just (mkL (l1+l2) `sub` x)

      -- l1 + (x - l2) ==> (l1-l2) + x
      (L l1, is_sub num_ops -> Just (x,L l2))
         -> Just (mkL (l1-l2) `add` x)

      -- (l1 + x) + (l2 + y) ==> (l1+l2) + (x+y)
      (is_lit_add num_ops -> Just (l1,x), is_lit_add num_ops -> Just (l2,y))
         -> Just (mkL (l1+l2) `add` (x `add` y))

      -- (l1 + x) + (l2 - y) ==> (l1+l2) + (x-y)
      (is_lit_add num_ops -> Just (l1,x), is_sub num_ops -> Just (L l2,y))
         -> Just (mkL (l1+l2) `add` (x `sub` y))

      -- (l1 + x) + (y - l2) ==> (l1-l2) + (x+y)
      (is_lit_add num_ops -> Just (l1,x), is_sub num_ops -> Just (y,L l2))
         -> Just (mkL (l1-l2) `add` (x `add` y))

      -- (l1 - x) + (l2 - y) ==> (l1+l2) - (x+y)
      (is_sub num_ops -> Just (L l1,x), is_sub num_ops -> Just (L l2,y))
         -> Just (mkL (l1+l2) `sub` (x `add` y))

      -- (l1 - x) + (y - l2) ==> (l1-l2) + (y-x)
      (is_sub num_ops -> Just (L l1,x), is_sub num_ops -> Just (y,L l2))
         -> Just (mkL (l1-l2) `add` (y `sub` x))

      -- (x - l1) + (y - l2) ==> (0-l1-l2) + (x+y)
      (is_sub num_ops -> Just (x,L l1), is_sub num_ops -> Just (y,L l2))
         -> Just (mkL (0-l1-l2) `add` (x `add` y))

      -- R4) Simple factorization

      -- x + x ==> 2 * x
      _ | Just l1 <- is_expr_mul num_ops arg1 arg2
        -> Just (mkL (l1+1) `mul` arg1)

      -- (l1 * x) + x ==> (l1+1) * x
      _ | Just l1 <- is_expr_mul num_ops arg2 arg1
        -> Just (mkL (l1+1) `mul` arg2)

      -- (l1 * x) + (l2 * x) ==> (l1+l2) * x
      (is_lit_mul num_ops -> Just (l1,x), is_expr_mul num_ops x -> Just l2)
         -> Just (mkL (l1+l2) `mul` x)

      -- R5) +/- propagation: these transformations push literals outwards
      -- with the hope that other rules can then be applied.

      -- In the following rules, x can't be a literal otherwise another
      -- rule would have combined it with the other literal in arg2. So we
      -- don't have to check this to avoid loops here.

      -- x + (l1 + y) ==> l1 + (x + y)
      (_, is_lit_add num_ops -> Just (l1,y))
         -> Just (mkL l1 `add` (arg1 `add` y))

      -- x + (l1 - y) ==> l1 + (x - y)
      (_, is_sub num_ops -> Just (L l1,y))
         -> Just (mkL l1 `add` (arg1 `sub` y))

      -- x + (y - l1) ==> (x + y) - l1
      (_, is_sub num_ops -> Just (y,L l1))
         -> Just ((arg1 `add` y) `sub` mkL l1)

      _ -> Nothing

   where
      mkL = Lit . mkNumLiteral platform num_ops
      add x y = BinOpApp x (numAdd num_ops) y
      sub x y = BinOpApp x (numSub num_ops) y
      mul x y = BinOpApp x (numMul num_ops) y

subFoldingRules' :: Platform -> CoreExpr -> CoreExpr -> NumOps -> Maybe CoreExpr
subFoldingRules' platform arg1 arg2 num_ops = case (arg1,arg2) of
      -- R1) +/- simplification

      -- l1 - (l2 + x) ==> (l1-l2) - x
      (L l1, is_lit_add num_ops -> Just (l2,x))
         -> Just (mkL (l1-l2) `sub` x)

      -- l1 - (l2 - x) ==> (l1-l2) + x
      (L l1, is_sub num_ops -> Just (L l2,x))
         -> Just (mkL (l1-l2) `add` x)

      -- l1 - (x - l2) ==> (l1+l2) - x
      (L l1, is_sub num_ops -> Just (x, L l2))
         -> Just (mkL (l1+l2) `sub` x)

      -- (l1 + x) - l2 ==> (l1-l2) + x
      (is_lit_add num_ops -> Just (l1,x), L l2)
         -> Just (mkL (l1-l2) `add` x)

      -- (l1 - x) - l2 ==> (l1-l2) - x
      (is_sub num_ops -> Just (L l1,x), L l2)
         -> Just (mkL (l1-l2) `sub` x)

      -- (x - l1) - l2 ==> x - (l1+l2)
      (is_sub num_ops -> Just (x,L l1), L l2)
         -> Just (x `sub` mkL (l1+l2))


      -- (l1 + x) - (l2 + y) ==> (l1-l2) + (x-y)
      (is_lit_add num_ops -> Just (l1,x), is_lit_add num_ops -> Just (l2,y))
         -> Just (mkL (l1-l2) `add` (x `sub` y))

      -- (l1 + x) - (l2 - y) ==> (l1-l2) + (x+y)
      (is_lit_add num_ops -> Just (l1,x), is_sub num_ops -> Just (L l2,y))
         -> Just (mkL (l1-l2) `add` (x `add` y))

      -- (l1 + x) - (y - l2) ==> (l1+l2) + (x-y)
      (is_lit_add num_ops -> Just (l1,x), is_sub num_ops -> Just (y,L l2))
         -> Just (mkL (l1+l2) `add` (x `sub` y))

      -- (l1 - x) - (l2 + y) ==> (l1-l2) - (x+y)
      (is_sub num_ops -> Just (L l1,x), is_lit_add num_ops -> Just (l2,y))
         -> Just (mkL (l1-l2) `sub` (x `add` y))

      -- (x - l1) - (l2 + y) ==> (0-l1-l2) + (x-y)
      (is_sub num_ops -> Just (x,L l1), is_lit_add num_ops -> Just (l2,y))
         -> Just (mkL (0-l1-l2) `add` (x `sub` y))

      -- (l1 - x) - (l2 - y) ==> (l1-l2) + (y-x)
      (is_sub num_ops -> Just (L l1,x), is_sub num_ops -> Just (L l2,y))
         -> Just (mkL (l1-l2) `add` (y `sub` x))

      -- (l1 - x) - (y - l2) ==> (l1+l2) - (x+y)
      (is_sub num_ops -> Just (L l1,x), is_sub num_ops -> Just (y,L l2))
         -> Just (mkL (l1+l2) `sub` (x `add` y))

      -- (x - l1) - (l2 - y) ==> (0-l1-l2) + (x+y)
      (is_sub num_ops -> Just (x,L l1), is_sub num_ops -> Just (L l2,y))
         -> Just (mkL (0-l1-l2) `add` (x `add` y))

      -- (x - l1) - (y - l2) ==> (l2-l1) + (x-y)
      (is_sub num_ops -> Just (x,L l1), is_sub num_ops -> Just (y,L l2))
         -> Just (mkL (l2-l1) `add` (x `sub` y))

       -- R4) Simple factorization

      -- x - (l1 * x) ==> (1-l1) * x
      _ | Just l1 <- is_expr_mul num_ops arg1 arg2
        -> Just (mkL (1-l1) `mul` arg1)

      -- (l1 * x) - x ==> (l1-1) * x
      _ | Just l1 <- is_expr_mul num_ops arg2 arg1
        -> Just (mkL (l1-1) `mul` arg2)

      -- (l1 * x) - (l2 * x) ==> (l1-l2) * x
      (is_lit_mul num_ops -> Just (l1,x), is_expr_mul num_ops x -> Just l2)
         -> Just (mkL (l1-l2) `mul` x)

      -- R5) +/- propagation: these transformations push literals outwards
      -- with the hope that other rules can then be applied.

      -- In the following rules, x can't be a literal otherwise another
      -- rule would have combined it with the other literal in arg2. So we
      -- don't have to check this to avoid loops here.

      -- x - (l1 + y) ==> (x - y) - l1
      (_, is_lit_add num_ops -> Just (l1,y))
         -> Just ((arg1 `sub` y) `sub` mkL l1)

      -- (l1 + x) - y ==> l1 + (x - y)
      (is_lit_add num_ops -> Just (l1,x), _)
         -> Just (mkL l1 `add` (x `sub` arg2))

      -- x - (l1 - y) ==> (x + y) - l1
      (_, is_sub num_ops -> Just (L l1,y))
         -> Just ((arg1 `add` y) `sub` mkL l1)

      -- x - (y - l1) ==> l1 + (x - y)
      (_, is_sub num_ops -> Just (y,L l1))
         -> Just (mkL l1 `add` (arg1 `sub` y))

      -- (l1 - x) - y ==> l1 - (x + y)
      (is_sub num_ops -> Just (L l1,x), _)
         -> Just (mkL l1 `sub` (x `add` arg2))

      -- (x - l1) - y ==> (x - y) - l1
      (is_sub num_ops -> Just (x,L l1), _)
         -> Just ((x `sub` arg2) `sub` mkL l1)

      _ -> Nothing
   where
      mkL = Lit . mkNumLiteral platform num_ops
      add x y = BinOpApp x (numAdd num_ops) y
      sub x y = BinOpApp x (numSub num_ops) y
      mul x y = BinOpApp x (numMul num_ops) y

mulFoldingRules' :: Platform -> CoreExpr -> CoreExpr -> NumOps -> Maybe CoreExpr
mulFoldingRules' platform arg1 arg2 num_ops = case (arg1,arg2) of
   -- l1 * (l2 * x) ==> (l1*l2) * x
   (L l1, is_lit_mul num_ops -> Just (l2,x))
      -> Just (mkL (l1*l2) `mul` x)

   -- l1 * (l2 + x) ==> (l1*l2) + (l1 * x)
   (L l1, is_lit_add num_ops -> Just (l2,x))
      -> Just (mkL (l1*l2) `add` (arg1 `mul` x))

   -- l1 * (l2 - x) ==> (l1*l2) - (l1 * x)
   (L l1, is_sub num_ops -> Just (L l2,x))
      -> Just (mkL (l1*l2) `sub` (arg1 `mul` x))

   -- l1 * (x - l2) ==> (l1 * x) - (l1*l2)
   (L l1, is_sub num_ops -> Just (x, L l2))
      -> Just ((arg1 `mul` x) `sub` mkL (l1*l2))

   -- (l1 * x) * (l2 * y) ==> (l1*l2) * (x * y)
   (is_lit_mul num_ops -> Just (l1,x), is_lit_mul num_ops -> Just (l2,y))
      -> Just (mkL (l1*l2) `mul` (x `mul` y))

   _ -> Nothing
   where
      mkL = Lit . mkNumLiteral platform num_ops
      add x y = BinOpApp x (numAdd num_ops) y
      sub x y = BinOpApp x (numSub num_ops) y
      mul x y = BinOpApp x (numMul num_ops) y

is_op :: PrimOp -> CoreExpr -> Maybe (Arg CoreBndr, Arg CoreBndr)
is_op op e = case e of
 BinOpApp x op' y | op == op' -> Just (x,y)
 _                            -> Nothing

is_add, is_sub, is_mul :: NumOps -> CoreExpr -> Maybe (Arg CoreBndr, Arg CoreBndr)
is_add num_ops = is_op (numAdd num_ops)
is_sub num_ops = is_op (numSub num_ops)
is_mul num_ops = is_op (numMul num_ops)

-- match addition with a literal (handles commutativity)
is_lit_add :: NumOps -> CoreExpr -> Maybe (Integer, Arg CoreBndr)
is_lit_add num_ops e = case is_add num_ops e of
   Just (L l, x  ) -> Just (l,x)
   Just (x  , L l) -> Just (l,x)
   _               -> Nothing

-- match multiplication with a literal (handles commutativity)
is_lit_mul :: NumOps -> CoreExpr -> Maybe (Integer, Arg CoreBndr)
is_lit_mul num_ops e = case is_mul num_ops e of
   Just (L l, x  ) -> Just (l,x)
   Just (x  , L l) -> Just (l,x)
   _               -> Nothing

-- match given "x": return 1
-- match "lit * x": return lit value (handles commutativity)
is_expr_mul :: NumOps -> Expr CoreBndr -> Expr CoreBndr -> Maybe Integer
is_expr_mul num_ops x e = if
   | x `cheapEqExpr` e
   -> Just 1
   | Just (k,x') <- is_lit_mul num_ops e
   , x `cheapEqExpr` x'
   -> return k
   | otherwise
   -> Nothing


-- | Match the application of a binary primop
pattern BinOpApp :: Arg CoreBndr -> PrimOp -> Arg CoreBndr -> CoreExpr
pattern BinOpApp x op y = OpVal op `App` x `App` y

-- | Match a primop
pattern OpVal:: PrimOp  -> Arg CoreBndr
pattern OpVal op <- Var (isPrimOpId_maybe -> Just op) where
   OpVal op = Var (mkPrimOpId op)

-- | Match a literal
pattern L :: Integer -> Arg CoreBndr
pattern L i <- Lit (LitNumber _ i)

-- | Explicit "type-class"-like dictionary for numeric primops
data NumOps = NumOps
   { numAdd     :: !PrimOp     -- ^ Add two numbers
   , numSub     :: !PrimOp     -- ^ Sub two numbers
   , numMul     :: !PrimOp     -- ^ Multiply two numbers
   , numLitType :: !LitNumType -- ^ Literal type
   }

-- | Create a numeric literal
mkNumLiteral :: Platform -> NumOps -> Integer -> Literal
mkNumLiteral platform ops i = mkLitNumberWrap platform (numLitType ops) i

int8Ops :: NumOps
int8Ops = NumOps
   { numAdd     = Int8AddOp
   , numSub     = Int8SubOp
   , numMul     = Int8MulOp
   , numLitType = LitNumInt8
   }

word8Ops :: NumOps
word8Ops = NumOps
   { numAdd     = Word8AddOp
   , numSub     = Word8SubOp
   , numMul     = Word8MulOp
   , numLitType = LitNumWord8
   }

int16Ops :: NumOps
int16Ops = NumOps
   { numAdd     = Int16AddOp
   , numSub     = Int16SubOp
   , numMul     = Int16MulOp
   , numLitType = LitNumInt16
   }

word16Ops :: NumOps
word16Ops = NumOps
   { numAdd     = Word16AddOp
   , numSub     = Word16SubOp
   , numMul     = Word16MulOp
   , numLitType = LitNumWord16
   }

int32Ops :: NumOps
int32Ops = NumOps
   { numAdd     = Int32AddOp
   , numSub     = Int32SubOp
   , numMul     = Int32MulOp
   , numLitType = LitNumInt32
   }

word32Ops :: NumOps
word32Ops = NumOps
   { numAdd     = Word32AddOp
   , numSub     = Word32SubOp
   , numMul     = Word32MulOp
   , numLitType = LitNumWord32
   }

#if WORD_SIZE_IN_BITS < 64
int64Ops :: NumOps
int64Ops = NumOps
   { numAdd     = Int64AddOp
   , numSub     = Int64SubOp
   , numMul     = Int64MulOp
   , numLitType = LitNumInt64
   }

word64Ops :: NumOps
word64Ops = NumOps
   { numAdd     = Word64AddOp
   , numSub     = Word64SubOp
   , numMul     = Word64MulOp
   , numLitType = LitNumWord64
   }
#endif

intOps :: NumOps
intOps = NumOps
   { numAdd     = IntAddOp
   , numSub     = IntSubOp
   , numMul     = IntMulOp
   , numLitType = LitNumInt
   }

wordOps :: NumOps
wordOps = NumOps
   { numAdd     = WordAddOp
   , numSub     = WordSubOp
   , numMul     = WordMulOp
   , numLitType = LitNumWord
   }

--------------------------------------------------------
-- Constant folding through case-expressions
--
-- cf Scrutinee Constant Folding in simplCore/GHC.Core.Opt.Simplify.Utils
--------------------------------------------------------

-- | Match the scrutinee of a case and potentially return a new scrutinee and a
-- function to apply to each literal alternative.
caseRules :: Platform
          -> CoreExpr                       -- Scrutinee
          -> Maybe ( CoreExpr               -- New scrutinee
                   , AltCon -> Maybe AltCon -- How to fix up the alt pattern
                                            --   Nothing <=> Unreachable
                                            -- See Note [Unreachable caseRules alternatives]
                   , Id -> CoreExpr)        -- How to reconstruct the original scrutinee
                                            -- from the new case-binder
-- e.g  case e of b {
--         ...;
--         con bs -> rhs;
--         ... }
--  ==>
--      case e' of b' {
--         ...;
--         fixup_altcon[con] bs -> let b = mk_orig[b] in rhs;
--         ... }

caseRules platform (App (App (Var f) v) (Lit l))   -- v `op` x#
  | Just op <- isPrimOpId_maybe f
  , LitNumber _ x <- l
  , Just adjust_lit <- adjustDyadicRight op x
  = Just (v, tx_lit_con platform adjust_lit
           , \v -> (App (App (Var f) (Var v)) (Lit l)))

caseRules platform (App (App (Var f) (Lit l)) v)   -- x# `op` v
  | Just op <- isPrimOpId_maybe f
  , LitNumber _ x <- l
  , Just adjust_lit <- adjustDyadicLeft x op
  = Just (v, tx_lit_con platform adjust_lit
           , \v -> (App (App (Var f) (Lit l)) (Var v)))


caseRules platform (App (Var f) v              )   -- op v
  | Just op <- isPrimOpId_maybe f
  , Just adjust_lit <- adjustUnary op
  = Just (v, tx_lit_con platform adjust_lit
           , \v -> App (Var f) (Var v))

-- See Note [caseRules for tagToEnum]
caseRules platform (App (App (Var f) type_arg) v)
  | Just TagToEnumOp <- isPrimOpId_maybe f
  = Just (v, tx_con_tte platform
           , \v -> (App (App (Var f) type_arg) (Var v)))

-- See Note [caseRules for dataToTag]
caseRules _ (App (App (Var f) (Type ty)) v)       -- dataToTag x
  | Just DataToTagOp <- isPrimOpId_maybe f
  , Just (tc, _) <- tcSplitTyConApp_maybe ty
  , isAlgTyCon tc
  = Just (v, tx_con_dtt ty
           , \v -> App (App (Var f) (Type ty)) (Var v))

caseRules _ _ = Nothing


tx_lit_con :: Platform -> (Integer -> Integer) -> AltCon -> Maybe AltCon
tx_lit_con _        _      DEFAULT    = Just DEFAULT
tx_lit_con platform adjust (LitAlt l) = Just $ LitAlt (mapLitValue platform adjust l)
tx_lit_con _        _      alt        = pprPanic "caseRules" (ppr alt)
   -- NB: mapLitValue uses mkLitIntWrap etc, to ensure that the
   -- literal alternatives remain in Word/Int target ranges
   -- (See Note [Word/Int underflow/overflow] in GHC.Types.Literal and #13172).

adjustDyadicRight :: PrimOp -> Integer -> Maybe (Integer -> Integer)
-- Given (x `op` lit) return a function 'f' s.t.  f (x `op` lit) = x
adjustDyadicRight op lit
  = case op of
         WordAddOp -> Just (\y -> y-lit      )
         IntAddOp  -> Just (\y -> y-lit      )
         WordSubOp -> Just (\y -> y+lit      )
         IntSubOp  -> Just (\y -> y+lit      )
         WordXorOp -> Just (\y -> y `xor` lit)
         IntXorOp  -> Just (\y -> y `xor` lit)
         _         -> Nothing

adjustDyadicLeft :: Integer -> PrimOp -> Maybe (Integer -> Integer)
-- Given (lit `op` x) return a function 'f' s.t.  f (lit `op` x) = x
adjustDyadicLeft lit op
  = case op of
         WordAddOp -> Just (\y -> y-lit      )
         IntAddOp  -> Just (\y -> y-lit      )
         WordSubOp -> Just (\y -> lit-y      )
         IntSubOp  -> Just (\y -> lit-y      )
         WordXorOp -> Just (\y -> y `xor` lit)
         IntXorOp  -> Just (\y -> y `xor` lit)
         _         -> Nothing


adjustUnary :: PrimOp -> Maybe (Integer -> Integer)
-- Given (op x) return a function 'f' s.t.  f (op x) = x
adjustUnary op
  = case op of
         WordNotOp -> Just (\y -> complement y)
         IntNotOp  -> Just (\y -> complement y)
         IntNegOp  -> Just (\y -> negate y    )
         _         -> Nothing

tx_con_tte :: Platform -> AltCon -> Maybe AltCon
tx_con_tte _        DEFAULT         = Just DEFAULT
tx_con_tte _        alt@(LitAlt {}) = pprPanic "caseRules" (ppr alt)
tx_con_tte platform (DataAlt dc)  -- See Note [caseRules for tagToEnum]
  = Just $ LitAlt $ mkLitInt platform $ toInteger $ dataConTagZ dc

tx_con_dtt :: Type -> AltCon -> Maybe AltCon
tx_con_dtt _  DEFAULT = Just DEFAULT
tx_con_dtt ty (LitAlt (LitNumber LitNumInt i))
   | tag >= 0
   , tag < n_data_cons
   = Just (DataAlt (data_cons !! tag))   -- tag is zero-indexed, as is (!!)
   | otherwise
   = Nothing
   where
     tag         = fromInteger i :: ConTagZ
     tc          = tyConAppTyCon ty
     n_data_cons = tyConFamilySize tc
     data_cons   = tyConDataCons tc

tx_con_dtt _ alt = pprPanic "caseRules" (ppr alt)


{- Note [caseRules for tagToEnum]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to transform
   case tagToEnum x of
     False -> e1
     True  -> e2
into
   case x of
     0# -> e1
     1# -> e2

This rule eliminates a lot of boilerplate. For
  if (x>y) then e2 else e1
we generate
  case tagToEnum (x ># y) of
    False -> e1
    True  -> e2
and it is nice to then get rid of the tagToEnum.

Beware (#14768): avoid the temptation to map constructor 0 to
DEFAULT, in the hope of getting this
  case (x ># y) of
    DEFAULT -> e1
    1#      -> e2
That fails utterly in the case of
   data Colour = Red | Green | Blue
   case tagToEnum x of
      DEFAULT -> e1
      Red     -> e2

We don't want to get this!
   case x of
      DEFAULT -> e1
      DEFAULT -> e2

Instead, we deal with turning one branch into DEFAULT in GHC.Core.Opt.Simplify.Utils
(add_default in mkCase3).

Note [caseRules for dataToTag]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [dataToTag#] in primpops.txt.pp

We want to transform
  case dataToTag x of
    DEFAULT -> e1
    1# -> e2
into
  case x of
    DEFAULT -> e1
    (:) _ _ -> e2

Note the need for some wildcard binders in
the 'cons' case.

For the time, we only apply this transformation when the type of `x` is a type
headed by a normal tycon. In particular, we do not apply this in the case of a
data family tycon, since that would require carefully applying coercion(s)
between the data family and the data family instance's representation type,
which caseRules isn't currently engineered to handle (#14680).

Note [Unreachable caseRules alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Take care if we see something like
  case dataToTag x of
    DEFAULT -> e1
    -1# -> e2
    100 -> e3
because there isn't a data constructor with tag -1 or 100. In this case the
out-of-range alternative is dead code -- we know the range of tags for x.

Hence caseRules returns (AltCon -> Maybe AltCon), with Nothing indicating
an alternative that is unreachable.

You may wonder how this can happen: check out #15436.


Note [Optimising conversions between numeric types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Converting between numeric types is very common in Haskell codes.  Suppose that
we have N inter-convertible numeric types (Word, Word8, Int, Integer, etc.).

- We don't want to have to use one conversion function per pair of types as that
would require N^2 functions: wordToWord8, wordToInt, wordToInteger...

- The following kind of class would allow us to have a single conversion
function at the price of N^2 instances and of the use of MultiParamTypeClasses
extension.

    class Convert a b where
      convert :: a -> b

What we do instead is that we use the Integer type (signed, unbounded) as a
passthrough type to perform every conversion. Hence we only need to define two
functions per numeric type:

  class Integral a where
    toInteger :: a -> Integer

  class Num a where
    fromInteger :: Integer -> a

These classes have a single parameter and can be derived automatically (e.g. for
newtypes). So we don't even have to define 2*N instances.

fromIntegral
------------

We can now define a generic conversion function:

  -- in the Prelude
  fromIntegral :: (Integral a, Num b) => a -> b
  fromIntegral = fromInteger . toInteger

The trouble with this approach is that performance might be terrible. E.g.
converting an Int into a Word, which is a no-op at the machine level, becomes
costly when performed via `fromIntegral` because an Integer has to be allocated.

To alleviate this:

- first `fromIntegral` was specialized (SPECIALIZE pragma). However it would
need N^2 pragmas to cover every case and it wouldn't cover user defined numeric
types which don't belong to base.

- while writing this note I discovered that we have a `-fwarn-identities` warning
to detect useless conversions (since 0656c72a8f):

  > fromIntegral (1 :: Int) :: Int

  <interactive>:3:21: warning: [-Widentities]
      Call of fromIntegral :: Int -> Int
        can probably be omitted

- but more importantly, many rules were added (e.g. in e0c787c10f):

    "fromIntegral/Int8->Int8" fromIntegral = id :: Int8 -> Int8
    "fromIntegral/a->Int8"    fromIntegral = \x -> case fromIntegral x of I# x# -> I8# (intToInt8# x#)
    "fromIntegral/Int8->a"    fromIntegral = \(I8# x#) -> fromIntegral (I# x#)

  The idea was to ensure that only cheap conversions ended up being used. E.g.:

      foo :: Int8 --> {- Integer -> -} -> Word8
      foo = fromIntegral

    ====> {Some fromIntegral rule for Int8}

      foo :: Int8 -> {- Int -> Integer -} -> Word8
      foo = fromIntegral . int8ToInt

    ====> {Some fromIntegral rule for Word8}

      foo :: Int8 -> {- Int -> Integer -> Word -} -> Word8
      foo = wordToWord8 . fromIntegral . int8ToInt

    ====> {Some fromIntegral rule for Int/Word}

      foo :: Int8 -> {- Int -> Word -} -> Word8
      foo = wordToWord8 . intToWord . int8ToInt
      -- not passing through Integer anymore!


It worked but there were still some issues with this approach:

1. These rules only work for `fromIntegral`. If we wanted to define our own
   similar function (e.g. using other type-classes), we would also have to redefine
   all the rules to get similar performance.

2. `fromIntegral` had to be marked `NOINLINE [1]`:
    - NOINLINE to allow rules to match
    - [1] to allow inlining in later phases to avoid incurring a function call
      overhead for such a trivial operation

   Users of the function had to be careful because a simple helper without an
   INLINE pragma like:

      toInt :: Integral a => a -> Int
      toInt = fromIntegral

   has the following unfolding:

      toInt = integerToInt . toInteger

   which doesn't mention `fromIntegral` anymore. Hence `fromIntegral` rules
   wouldn't be triggered for any user of `toInt`. For this reason, we also have
   a bunch of rules for bignum primitives such as `integerToInt`.

3. These rewrite rules are tedious to write and error-prone (cf #19345).


For these reasons, it is simpler to only rely on built-in rewrite rules for
bignum primitives. There aren't so many conversion primitives:
  - Natural <-> Word
  - Integer <-> Int/Word/Natural (+ Int64/Word64 on 32-bit arch)

All the built-in "small_passthrough_*" rules are used to avoid passing through
Integer/Natural. We now always inline `fromIntegral`.

-}
