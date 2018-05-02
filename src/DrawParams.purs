module DrawParams where

import Prelude

import AnimPos (Speed, Pos)
import Data.Int (toNumber)
import Vect (Vect(..))

create :: Number -> Number -> Settings
create width height =
  { canvasWidth : width
  , canvasHeight : height
  , blockWidth : bW
  , blockHeight : bH
  , speed : 1.25 * width / 1000.0
  , toPos : \col row -> Vect (toNumber col * bW) (toNumber row * bH)
  }
  where 
    bW = width / 4.0
    bH = height / 4.0

type Settings =
  { canvasWidth :: Number
  , canvasHeight :: Number
  , blockWidth :: Number
  , blockHeight :: Number
  , speed :: Speed
  , toPos :: Int -> Int -> Pos
  }