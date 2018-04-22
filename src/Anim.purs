module Anim 
  ( Anim, Pos, Dir, Speed
  , class HasPos, getPos, setPos
  , getX, getY
  , current
  , isMoving
  , initStatic
  , moveTo
  , animate
  ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Vect (Vect, isNear, (*.), (+.))
import Vect as Vect

type Pos = Vect
type Dir = Vect
type Speed = Number


class HasPos a where
  getPos :: a -> Pos
  setPos :: Pos -> a -> a


getX :: forall a . HasPos a => a -> Number
getX a = let (Vect.Vect x _) = getPos a in x


getY :: forall a . HasPos a => a -> Number
getY a = let (Vect.Vect _ y) = getPos a in y


data Anim a
    = Static a
    | Animated a (Milliseconds -> Anim a)


current :: forall a . Anim a -> a
current (Static a) = a
current (Animated a _) = a


isMoving :: forall a . Anim a -> Boolean
isMoving (Static _) = false 
isMoving (Animated _ _) = true 


initStatic :: forall a . a -> Anim a
initStatic = Static


animate :: forall a . HasPos a => Milliseconds -> Anim a -> Anim a
animate _ a@(Static _) = a
animate delta (Animated _ anim) = anim delta


moveTo :: forall a . HasPos a => Speed -> Pos -> Anim a -> Anim a
moveTo maxSpeed toPos anim
    | Vect.isNear (getPos $ current anim) toPos = Static obj
    where
      obj = current anim
    | otherwise = 
        Animated obj (moveTo obj 0.0 (Vect.direction (getPos obj) toPos))
    where
      obj = current anim
      moveTo curObj lastSpeed lastDir (Milliseconds delta) =
          let
            speed = max maxSpeed (lastSpeed + maxSpeed / 20.0)
            nextPos = getPos curObj +. delta * speed *. lastDir
            newDir = Vect.direction nextPos toPos
            nextObj = setPos nextPos curObj
          in 
            if isNear newDir lastDir then
              Animated nextObj (moveTo nextObj speed lastDir)                
            else
              Static (setPos toPos curObj)
