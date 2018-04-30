module AnimPos 
  ( Pos, Dir, Speed
  , class HasPos, getPos, setPos
  , getX, getY
  , moveTo
  ) where

import Prelude

import Anim (Anim, current, fromCont)
import Data.Time.Duration (Milliseconds(..))
import Vect (Vect, isNear, (*.), (+.))
import Vect as Vect

type Pos = Vect
type Dir = Vect
type Speed = Number


class HasPos a where
  getPos :: a -> Pos
  setPos :: Pos -> a -> a


instance vectHasPos :: HasPos Vect where
  getPos = id
  setPos = const  


getX :: forall a . HasPos a => a -> Number
getX a = let (Vect.Vect x _) = getPos a in x


getY :: forall a . HasPos a => a -> Number
getY a = let (Vect.Vect _ y) = getPos a in y


moveTo :: forall a . HasPos a => Speed -> Pos -> Anim a -> Anim a
moveTo maxSpeed toPos anim
    | Vect.isNear (getPos $ current anim) toPos = pure obj
    where
      obj = current anim
    | otherwise = 
        fromCont obj (moveTo obj 0.0 (Vect.direction (getPos obj) toPos))
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
              fromCont nextObj (moveTo nextObj speed lastDir)                
            else
              pure (setPos toPos curObj)
