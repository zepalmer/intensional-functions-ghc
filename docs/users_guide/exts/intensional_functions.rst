.. _intensional-functions:

Intensional Functions
---------------------

.. index::
   single: intensional functions
   single: intensional

.. extension:: IntensionalFunctions
    :shortdesc: Enable syntax and desugaring for intensional functions.

    :since: 9.2.1

    Enable the syntax and desugaring for intensional functions.

Typical functions in Haskell and other functional programming languages are *extensional*: they are distinct and incomparable.  Intensional functions are defined in terms of some amount of their structure, in this case their source definitions and closures.  The ``IntensionalFunctions`` extension permits programmers to create and manipulate intensional functions and to subject those functions to appropriate constraints.  An intensional function may be ``Eq`` or ``Ord`` or satisfy any other constraint so long as a definition can be provided in terms of the definition site and the function's closure.  For reasons of decidability, these instances are, of course, conservative approximations; nonetheless, intensional functions allow abstract computations to be examined and treated as first-class values, allowing a sort of automatic defunctionalization.

The ``IntensionalFunctions`` extension provides multiple forms of syntactic sugar.  Probably the most intuitive is the *curried* intensional function demonstrated in the following example: ::

    {-# LANGUAGE IntensionalFunctions #-}
    add :: Int ->%Eq Int ->%Eq -> Int
    add = \%%Eq x y -> x + y
    sub :: Int ->%Eq Int ->%Eq -> Int
    sub = \%%Eq x y -> x - y

The above code defines two functions, each of which have two parameters.  The first function sums its parameters' values; the second calculates its parameters' values' difference.  Because these functions are intensional and subject to the ``Eq`` constraint function, operations such as ``add == add`` or ``add == sub`` are valid.  Partial applications of these functions are also intensional: the expression ``add 4 == add 5`` typechecks and evaluates to ``False`` because those two expressions cannot be proven to produce identical results in all cases.  The constraint function ``Eq`` is of kind ``Type -> Constraint`` and is applied to both the function itself (allowing it to be compared for equality) and to its closure (requiring that all values captured in closure have definitions of ``Eq``).

Intensional Monads
------------------

In addition to the type and expression syntax above, the ``IntensionalFunctions`` extension provides library definitions for algebraic constructions such as monads defined in terms of intensional functions.  Correspondingly, the extension allows for intensional monad syntax.  For instance: ::

    {-# LANGUAGE IntensionalFunctions #-}
    import Control.Intensional.Monad.State

    foo :: State Eq Int Int ->%Eq State Eq Int Int
    foo = \%Eq mx -> intensional Eq do
      x <- mx
      y <- get
      put x
      ipure %@ y

Each statement in this intensional do-block uses intensional monad oerations (rather than the traditional extensional ones).  The function ``ipure``, for instance, is the intensional applicative analogue to ``pure``.  The ``<-`` bind operations in this block use ``ibind``, the intensional analogue of ``>>=``.  Because intensional do-blocks use intensional monadic operations, any well-typed intensional do-block is itself subject to the constraint it describes.  Given the above code, for instance, ``foo x == foo y`` is well-typed as long as ``x`` and ``y`` are intensional state-monadic actions with integer state and integer result (even though intensional state is, as expected, defined in terms of (intensional) functions).

Uncurried Intensional Functions
-------------------------------

If an intensional function's argument is captured in closure, it must be subject to the intensional function's constraint function (e.g. ``Eq``).  In some cases, however, this is not appropriate.  In the case of an intensional monad bind, for instance, we might be tempted to give the type ::

    itsBind :: (IntensionalMonad m, c ~ IntensionalMonadBindC m, ...)
            => m a ->%c (a ->%c m b) ->%c m b

(We elide some constraints not related to the discussion here.)  If we use this type, however, the value of type ``m a`` will be captured in the partial application of ``itsBind``, requiring the constraint ``c (m a)`` to hold.  This is an onerous requirement, especially as both arguments to ``itsBind`` will typically be applied at the same program point.  To address this, the ``IntensionalFunctions`` extension provides *uncurried* intensional functions which indicate the specific set of arguments necessary to reach a point of saturation.  If all of these arguments are applied simultaneously, none of them are captured in closure and so none are subject to the constraint function at hand.  For instance, we can give ``itsBind`` a type like ::

    itsBind :: (IntensionalMonad m, c ~ IntensionalMonadBindC m, ...)
            => '[m a, a ->%c m b] ->%%c m b

Here, the ``->%%c`` notation indicates an uncurried intensional function with a left operand of kind ``[Type]``.  This list contains all of the argument types necessary to saturate ``itsBind``.  If both arguments are applied simultaneously, the ``m b`` is produced without requiring ``c (m a)`` to hold.  However, the application operators are defined to permit the call to ``itsBind`` to undersaturate, in which case the rules for curried intensional functions apply.  Uncurried intensional functions are strictly more powerful than curried intensional functions but provide a somewhat more cumbersome programmer interface.
