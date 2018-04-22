module Vect where

import Prelude

import Math (sqrt)

data Vect = Vect Number Number

derive instance eqVect :: Eq Vect


direction :: Vect -> Vect -> Vect
direction v1 v2 =
    let dir = v2 -. v1
    in (1.0 / vLength dir) *. dir


isNear :: Vect -> Vect -> Boolean
isNear v1 v2 = dist v1 v2 <= 0.001


dist :: Vect -> Vect -> Number
dist v1 v2 = vLength (v1 -. v2)


vAdd :: Vect -> Vect -> Vect
vAdd (Vect x y) (Vect x' y') = Vect (x+x') (y+y')
infix 6 vAdd as +.


vDiff :: Vect -> Vect -> Vect
vDiff (Vect x y) (Vect x' y') = Vect (x-x') (y-y')
infix 6 vDiff as -.


scalMul :: Number -> Vect -> Vect
scalMul s (Vect x y) = Vect (s * x) (s * y)
infixl 7 scalMul as *.


vLength :: Vect -> Number
vLength (Vect x y) = sqrt (x*x + y*y)