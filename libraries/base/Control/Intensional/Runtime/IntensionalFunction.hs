{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Intensional.Runtime.IntensionalFunction
( -- * Intensional function definitions
  Label(..)
, ExtensionalFunctionF
, ClosureItem(..)
, IntensionalFunction(..)
, itsForget
, itsIdentify
, itsInspect
  -- * Intensional function application
, SaturationLevel(..)
, IntensionalFunctionApplicationSaturation
, IntensionalFunctionSingleApplication(..)
, IntensionalFunctionMultiApplication(..)
, IntensionalFunctionMultiApplicationOverloadArgumentMap
, IntensionalFunctionMultiApplicationOverload(..)
) where

import Data.Typeable

import Control.Intensional.Runtime.Constraints
import Control.Intensional.Runtime.NonEmptyHList
import Control.Intensional.Runtime.NonEmptyList

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Definition of intensional functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -}

data Label = Label !Int -- a hash of relevant location information
    deriving (Eq, Ord, Show)

type family ExtensionalFunctionF
        (inputs :: NonEmptyList Type) (output :: Type) :: Type where
    ExtensionalFunctionF ('NonEmptyListSingleton input) output =
        input -> output
    ExtensionalFunctionF ('NonEmptyListCons input inputs) output =
        input -> ExtensionalFunctionF inputs output

data ClosureItem (cfn :: ConstraintFn) where
    ClosureItem :: forall cfn a. (Typeable a, cfn a) => a -> ClosureItem cfn

data IntensionalFunction
        (cfn :: ConstraintFn)
        (inputs :: NonEmptyList Type)
        (output :: Type) where
    IntensionalFunction :: Label
                        -> [ClosureItem cfn]
                        -> ExtensionalFunctionF inputs output
                        -> IntensionalFunction cfn inputs output

itsForget :: IntensionalFunction cfn inputs output
          -> ExtensionalFunctionF inputs output
itsForget (IntensionalFunction _ _ fn) = fn

itsIdentify :: IntensionalFunction cfn inputs output -> Label
itsIdentify (IntensionalFunction lbl _ _) = lbl

itsInspect :: IntensionalFunction cfn inputs output -> [ClosureItem cfn]
itsInspect (IntensionalFunction _ closure _) = closure

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Instances on intensional functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -}

instance Eq (ClosureItem Eq) where
    (==) (ClosureItem x) (ClosureItem y) =
        case cast x of
            Just x' -> x' == y
            Nothing -> False

instance Eq (ClosureItem Ord) where
    (==) (ClosureItem x) (ClosureItem y) =
        case cast x of
            Just x' -> x' == y
            Nothing -> False

instance Ord (ClosureItem Ord) where
    compare (ClosureItem x) (ClosureItem y) =
        case cast x of
            Just x' -> x' `compare` y
            Nothing -> typeOf x `compare` typeOf y

instance Eq (IntensionalFunction Eq inputs output) where
    (==)
        (IntensionalFunction lbl closure _)
        (IntensionalFunction lbl' closure' _) =
            lbl == lbl' && closure == closure'

instance Eq (IntensionalFunction Ord inputs output) where
    (==)
        (IntensionalFunction lbl closure _)
        (IntensionalFunction lbl' closure' _) =
            lbl == lbl' && closure == closure'

instance Ord (IntensionalFunction Ord inputs output) where
    compare
        (IntensionalFunction lbl closure _)
        (IntensionalFunction lbl' closure' _) =
            (lbl,closure) `compare` (lbl',closure')

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Definition of intensional function application.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -}

undersaturatingIntensionalSingleApplication ::
       (cfn arg, Typeable arg)
    => IntensionalFunction cfn ('NonEmptyListCons arg inputs) output
    -> arg
    -> IntensionalFunction cfn inputs output
undersaturatingIntensionalSingleApplication
        (IntensionalFunction lbl clo fn) arg =
    IntensionalFunction lbl (ClosureItem arg : clo) (fn arg)

undersaturatingIntensionalApplication ::
       ( NonEmptyListConstrainAll cfn args
       , NonEmptyListConstrainAll Typeable args
       )
    => IntensionalFunction cfn (NonEmptyListConcat args inputs) output
    -> NonEmptyHList args
    -> IntensionalFunction cfn inputs output
undersaturatingIntensionalApplication
        (IntensionalFunction lbl clo fn) args =
    case args of
        NonEmptyHListSingleton arg ->
            IntensionalFunction lbl (ClosureItem arg : clo) (fn arg)
        NonEmptyHListCons arg args' ->
            let ifn' =
                  IntensionalFunction lbl (ClosureItem arg : clo) (fn arg)
            in
            undersaturatingIntensionalApplication ifn' args'

saturatingIntensionalApplication ::
       forall cfn inputs output.
       IntensionalFunction cfn inputs output
    -> NonEmptyHList inputs
    -> output
saturatingIntensionalApplication (IntensionalFunction _ _ fnIn) argsIn =
    let apply :: forall inputs'.
                 ExtensionalFunctionF inputs' output
              -> NonEmptyHList inputs'
              -> output
        apply fn args =
            case args of
                NonEmptyHListSingleton arg -> fn arg
                NonEmptyHListCons arg args' -> apply (fn arg) args'
    in
    apply fnIn argsIn

oversaturatingIntensionalApplicationStep ::
       forall cfn inputs output args.
       (SplitNonEmptyHList inputs args)
    => IntensionalFunction cfn inputs output
    -> NonEmptyHList (NonEmptyListConcat inputs args)
    -> (output, NonEmptyHList args)
oversaturatingIntensionalApplicationStep ifn args =
    let saturatingArgs :: NonEmptyHList inputs
        extraArgs :: NonEmptyHList args
        (saturatingArgs, extraArgs) = splitNonEmptyHList args
    in
    let output = saturatingIntensionalApplication ifn saturatingArgs in
    (output, extraArgs)

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Definition of intensional function application operators.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -}

data SaturationLevel
    = Saturated
    | Undersaturated
    | Oversaturated SaturationLevel

type family IntensionalFunctionApplicationSaturation
        (inputs :: NonEmptyList Type)
        (output :: Type)
        (args :: NonEmptyList Type)
        :: SaturationLevel where
    -- Saturation
    IntensionalFunctionApplicationSaturation
        ('NonEmptyListSingleton input)
        output
        ('NonEmptyListSingleton arg)
        = 'Saturated
    -- Undersaturation
    IntensionalFunctionApplicationSaturation
        ('NonEmptyListCons input inputs)
        output
        ('NonEmptyListSingleton arg)
        = 'Undersaturated
    -- Oversaturation
    IntensionalFunctionApplicationSaturation
        ('NonEmptyListSingleton input)
        (IntensionalFunction cfn inps outp)
        ('NonEmptyListCons arg args)
        = 'Oversaturated
            (IntensionalFunctionApplicationSaturation inps outp args)
    -- Tandem induction
    IntensionalFunctionApplicationSaturation
        ('NonEmptyListCons input inputs)
        output
        ('NonEmptyListCons arg args)
        = IntensionalFunctionApplicationSaturation inputs output args

infixl 9 %@
infixr 1 %$

class IntensionalFunctionSingleApplication
        (cfn :: ConstraintFn)
        (inputs :: NonEmptyList Type)
        (output :: Type)
        (arg :: Type) where
    type IntensionalFunctionSingleApplicationResult
            cfn inputs output arg :: Type
    (%@) :: IntensionalFunction cfn inputs output
         -> arg
         -> IntensionalFunctionSingleApplicationResult cfn inputs output arg
    (%$) :: IntensionalFunction cfn inputs output
         -> arg
         -> IntensionalFunctionSingleApplicationResult cfn inputs output arg
    (%$) = (%@)

instance (arg ~ input) => IntensionalFunctionSingleApplication
        cfn
        ('NonEmptyListSingleton input)
        output
        arg where
    type IntensionalFunctionSingleApplicationResult
            cfn ('NonEmptyListSingleton input) output arg
        = output
    (%@) ifn arg =
        ifn `saturatingIntensionalApplication` (NonEmptyHListSingleton arg)

instance (arg ~ input, cfn input, Typeable input)
    => IntensionalFunctionSingleApplication
        cfn
        ('NonEmptyListCons input inputs)
        output
        arg where
    type IntensionalFunctionSingleApplicationResult
            cfn ('NonEmptyListCons input inputs) output arg
        = IntensionalFunction cfn inputs output
    (%@) = undersaturatingIntensionalSingleApplication

infixl 9 %@+
infixr 1 %$+

class (saturation ~ IntensionalFunctionApplicationSaturation inputs output args)
    => IntensionalFunctionMultiApplication
        (cfn :: ConstraintFn)
        (inputs :: NonEmptyList Type)
        (output :: Type)
        (args :: NonEmptyList Type)
        (saturation :: SaturationLevel) where
    type IntensionalFunctionMultiApplicationResult
        cfn inputs output args saturation :: Type
    (%@+) :: IntensionalFunction cfn inputs output
          -> NonEmptyHList args
          -> IntensionalFunctionMultiApplicationResult
                cfn inputs output args saturation
    (%$+) :: IntensionalFunction cfn inputs output
          -> NonEmptyHList args
          -> IntensionalFunctionMultiApplicationResult
                cfn inputs output args saturation
    (%$+) = (%@+)

instance ( 'Saturated ~
             IntensionalFunctionApplicationSaturation inputs output args
         , inputs ~ args
         )
    => IntensionalFunctionMultiApplication
        cfn
        inputs
        output
        args
        'Saturated where
    type IntensionalFunctionMultiApplicationResult
            cfn inputs output args 'Saturated =
        output
    (%@+) = saturatingIntensionalApplication

instance ( 'Undersaturated ~
             IntensionalFunctionApplicationSaturation inputs output args
         , (NonEmptyListConcat args (NonEmptyListSuffix args inputs)) ~ inputs
         , NonEmptyListConstrainAll cfn args
         , NonEmptyListConstrainAll Typeable args
         )
    => IntensionalFunctionMultiApplication
        cfn
        inputs
        output
        args
        'Undersaturated where
    type IntensionalFunctionMultiApplicationResult
            cfn inputs output args 'Undersaturated =
        IntensionalFunction cfn (NonEmptyListSuffix args inputs) output
    (%@+) = undersaturatingIntensionalApplication

instance ( 'Oversaturated saturation ~
             IntensionalFunctionApplicationSaturation
                inputs
                (IntensionalFunction cfn inps outp)
                args
         , IntensionalFunctionMultiApplication
             cfn inps outp (NonEmptyListSuffix inputs args) saturation
         , (NonEmptyListConcat inputs (NonEmptyListSuffix inputs args)) ~ args
         , SplitNonEmptyHList inputs (NonEmptyListSuffix inputs args)
         )
    => IntensionalFunctionMultiApplication
        cfn
        inputs
        (IntensionalFunction cfn inps outp)
        args
        ('Oversaturated saturation) where
    type IntensionalFunctionMultiApplicationResult
            cfn
            inputs
            (IntensionalFunction cfn inps outp)
            args
            ('Oversaturated saturation) =
        IntensionalFunctionMultiApplicationResult
            cfn
            inps
            outp
            (NonEmptyListSuffix inputs args)
            saturation
    (%@+) ifn args =
        let (ifn', args') =
                oversaturatingIntensionalApplicationStep
                    @cfn
                    @inputs
                    @(IntensionalFunction cfn inps outp)
                    @(NonEmptyListSuffix inputs args)
                    ifn args
        in
        ifn' %@+ args'

type family IntensionalFunctionMultiApplicationOverloadArgumentMap
        (args :: Type)
    :: NonEmptyList Type

infixl 9 %@%
infixr 1 %$%

class ( IntensionalFunctionMultiApplication
          cfn
          inputs
          output
          (IntensionalFunctionMultiApplicationOverloadArgumentMap args)
          saturation
      )
    => IntensionalFunctionMultiApplicationOverload
        (cfn :: ConstraintFn)
        (inputs :: NonEmptyList Type)
        (output :: Type)
        (args :: Type)
        (saturation :: SaturationLevel) where
    (%@%) :: IntensionalFunction cfn inputs output
          -> args
          -> IntensionalFunctionMultiApplicationResult
                cfn
                inputs
                output
                (IntensionalFunctionMultiApplicationOverloadArgumentMap args)
                saturation
    (%$%) :: IntensionalFunction cfn inputs output
          -> args
          -> IntensionalFunctionMultiApplicationResult
                cfn
                inputs
                output
                (IntensionalFunctionMultiApplicationOverloadArgumentMap args)
                saturation
    (%$%) = (%@%)

type instance IntensionalFunctionMultiApplicationOverloadArgumentMap
        (t1,t2) =
    'NonEmptyListCons t1 ('NonEmptyListSingleton t2)
instance (IntensionalFunctionMultiApplication
            cfn
            inputs
            output
            (IntensionalFunctionMultiApplicationOverloadArgumentMap (t1,t2))
            saturation)
    => IntensionalFunctionMultiApplicationOverload
            cfn inputs output (t1,t2) saturation where
    (%@%) fn (arg1,arg2) =
        (%@+) fn (NonEmptyHListCons arg1 $
                    NonEmptyHListSingleton arg2)

type instance IntensionalFunctionMultiApplicationOverloadArgumentMap
        (t1,t2,t3) =
    'NonEmptyListCons t1 ('NonEmptyListCons t2 ('NonEmptyListSingleton t3))
instance (IntensionalFunctionMultiApplication
            cfn
            inputs
            output
            (IntensionalFunctionMultiApplicationOverloadArgumentMap (t1,t2,t3))
            saturation)
    => IntensionalFunctionMultiApplicationOverload
            cfn inputs output (t1,t2,t3) saturation where
    (%@%) fn (arg1,arg2,arg3) =
        (%@+) fn (NonEmptyHListCons arg1 $
                    NonEmptyHListCons arg2 $
                        NonEmptyHListSingleton arg3)

type instance IntensionalFunctionMultiApplicationOverloadArgumentMap
        (t1,t2,t3,t4) =
    'NonEmptyListCons t1
        ('NonEmptyListCons t2
            ('NonEmptyListCons t3
                ('NonEmptyListSingleton t4)))
instance (IntensionalFunctionMultiApplication
            cfn
            inputs
            output
            (IntensionalFunctionMultiApplicationOverloadArgumentMap
                (t1,t2,t3,t4))
            saturation)
    => IntensionalFunctionMultiApplicationOverload
            cfn inputs output (t1,t2,t3,t4) saturation where
    (%@%) fn (arg1,arg2,arg3,arg4) =
        (%@+) fn (NonEmptyHListCons arg1 $
                    NonEmptyHListCons arg2 $
                        NonEmptyHListCons arg3 $
                            NonEmptyHListSingleton arg4)

type instance IntensionalFunctionMultiApplicationOverloadArgumentMap
        (t1,t2,t3,t4,t5) =
    'NonEmptyListCons t1
        ('NonEmptyListCons t2
            ('NonEmptyListCons t3
                ('NonEmptyListCons t4
                    ('NonEmptyListSingleton t5))))
instance (IntensionalFunctionMultiApplication
            cfn
            inputs
            output
            (IntensionalFunctionMultiApplicationOverloadArgumentMap
                (t1,t2,t3,t4,t5))
            saturation)
    => IntensionalFunctionMultiApplicationOverload
            cfn inputs output (t1,t2,t3,t4,t5) saturation where
    (%@%) fn (arg1,arg2,arg3,arg4,arg5) =
        (%@+) fn (NonEmptyHListCons arg1 $
                    NonEmptyHListCons arg2 $
                        NonEmptyHListCons arg3 $
                            NonEmptyHListCons arg4 $
                                NonEmptyHListSingleton arg5)

type instance IntensionalFunctionMultiApplicationOverloadArgumentMap
        (t1,t2,t3,t4,t5,t6) =
    'NonEmptyListCons t1
        ('NonEmptyListCons t2
            ('NonEmptyListCons t3
                ('NonEmptyListCons t4
                    ('NonEmptyListCons t5
                        ('NonEmptyListSingleton t6)))))
instance (IntensionalFunctionMultiApplication
            cfn
            inputs
            output
            (IntensionalFunctionMultiApplicationOverloadArgumentMap
                (t1,t2,t3,t4,t5,t6))
            saturation)
    => IntensionalFunctionMultiApplicationOverload
            cfn inputs output (t1,t2,t3,t4,t5,t6) saturation where
    (%@%) fn (arg1,arg2,arg3,arg4,arg5,arg6) =
        (%@+) fn (NonEmptyHListCons arg1 $
                    NonEmptyHListCons arg2 $
                        NonEmptyHListCons arg3 $
                            NonEmptyHListCons arg4 $
                                NonEmptyHListCons arg5 $
                                    NonEmptyHListSingleton arg6)

{-
TODO: an operator that takes
  '[x,y,...] ->%%c r
and produces
  x ->%c ('[y,...] ->%%c r)
-}