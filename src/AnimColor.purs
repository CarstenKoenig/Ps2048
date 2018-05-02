module AnimColor 
  ( Color
  , create, toCssColor
  , class HasColor, getColor, setColor
  , changeColorTo
  ) where

import Prelude

import Anim (Anim, current, fromCont)
import Data.Int (round, toNumber)
import Data.Time.Duration (Milliseconds(..))


data Color = Color
  { r :: Number
  , g :: Number
  , b :: Number
  }

derive instance eqColor :: Eq Color


create :: Int -> Int -> Int -> Color
create r g b = Color
  { r: toNumber r
  , g: toNumber g
  , b: toNumber b 
  }


toCssColor :: Color -> String
toCssColor (Color color) = 
  "rgb(" 
    <> show (round color.r) <> "," 
    <> show (round color.g) <> "," 
    <> show (round color.b) <> ")"


interpolate :: Color -> Color -> Number -> Color
interpolate (Color colFrom) (Color colTo) t = 
  Color
    { r: inter colFrom.r colTo.r
    , g: inter colFrom.g colTo.g
    , b: inter colFrom.b colTo.b
    }
  where inter a b = a + (b-a) * t

class HasColor a where
  getColor :: a -> Color
  setColor :: Color -> a -> a


instance colorHasColor :: HasColor Color where
  getColor = id
  setColor = const  


changeColorTo :: forall a . HasColor a => Milliseconds -> Color -> Anim a -> Anim a
changeColorTo (Milliseconds duration) toColor anim
    | getColor (current anim) == toColor = pure (current anim)
    | otherwise = 
        fromCont obj (colorTo obj 0.0)
    where
      obj = current anim
      startColor = getColor obj
      colorTo curObj lastTime (Milliseconds delta) =
          let
            time = min duration (lastTime + delta)
            nextColor = interpolate startColor toColor (time/duration)
            nextObj = setColor nextColor curObj
          in 
            if time < duration then
              fromCont nextObj (colorTo nextObj time)                
            else
              pure (setColor toColor curObj)
