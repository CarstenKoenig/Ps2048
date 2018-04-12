module Block where

import Prelude

import Data.Maybe (Maybe(..))

type Color = String

type Pos = { x :: Number, y :: Number }

type HasPos r = { x :: Number, y :: Number | r }

data Point = Point
    { x :: Number
    , y :: Number
    , vx :: Number
    , vy :: Number 
    , ax :: Number
    , ay :: Number
    }

data Block = Block
    { color :: Color
    , x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
    , value :: Int
    , animPos :: Maybe Point
    }

class IsAnimated o where
    animate :: (Point -> Point) -> o -> Maybe o

instance pointIsAnimated :: IsAnimated Point where
    animate anim pt = Just (anim pt)

instance blockIsAnimated :: IsAnimated Block where
    animate anim (Block bl) =
        case bl.animPos of
            Nothing -> Nothing
            Just pt -> Just $ Block $ bl { animPos = animate anim pt }