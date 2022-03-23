{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE TypeApplications #-}

module Control.Intensional.UtilityFunctions (
  itsId,
  itsCompose,
  itsFlip,
  itsFirst,
  itsSecond,
  itsCurry1
) where

import Control.Intensional.Runtime
import Control.Intensional.Runtime.NonEmptyList

itsId :: forall c a. a ->%c a
itsId = \%%c x -> x

itsCompose :: forall c x y z.
              ( Typeable c, Typeable x, Typeable y, Typeable z
              , c (y ->%c z)
              , c (x ->%c y)
              )
           => [ y ->%c z
              , x ->%c y
              , x
              ]
        ->%%c z
itsCompose = \%%c f g x -> f %$ g %@ x

itsFlip :: forall c x y z.
           '[ '[x,y] ->%%c z
            , y
            , x
            ] ->%%c z
itsFlip = \%%c f x y -> f %@+ (NonEmptyHListCons y (NonEmptyHListSingleton x))

itsFirst :: forall c x y z.
            '[ (x ->%c z)
             , (x,y)
             ] ->%%c (z,y)
itsFirst = \%%c f (x,y) -> (f %@ x, y)

itsSecond :: forall c x y z.
             '[ (y ->%c z)
              , (x,y)
              ] ->%%c (x,z)
itsSecond = \%%c f (x,y) -> (x, f %@ y)

itsCurry1 :: forall c c' input inputs output.
             ( Typeable c, Typeable input, Typeable inputs, Typeable output
             , c (IntensionalFunction c ('NonEmptyListCons input inputs) output)
             , c input
             )
          => IntensionalFunction c ('NonEmptyListCons input inputs) output
       ->%c' IntensionalFunction c ('NonEmptyListSingleton input)
               (IntensionalFunction c inputs output)
itsCurry1 = \%c' ifn -> \%c x -> ifn %@ x