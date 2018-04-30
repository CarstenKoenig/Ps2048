module Block where

import Prelude

import Anim (class HasPos, Anim, Pos, initStatic)
import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS, Context2D, beginPath, closePath, fillRect, setFillStyle)
import Vect (Vect(..))

type Color = String

newtype Block = Block
    { color :: Color
    , width :: Number
    , height :: Number
    , value :: Int
    , pos :: Pos
    }


create :: Color -> Number -> Number -> Int -> Pos -> Anim Block
create col wdt hgt val p =
    initStatic $ Block
        { color: col
        , width: wdt
        , height: hgt
        , value: val
        , pos: p
        }


instance hasPosBlock :: HasPos Block where
    getPos (Block b) = b.pos
    setPos p (Block b) = Block (b { pos = p } )


sameValue :: Block -> Block -> Boolean
sameValue (Block b1) (Block b2) = 
    b1.value == b2.value


draw :: forall eff . Context2D -> Block -> Eff ( canvas :: CANVAS | eff ) Unit
draw ctx (Block b) = do
    void $ beginPath ctx
    void $ setFillStyle b.color ctx
    void $ fillRect ctx rect
    void $ closePath ctx
    where
        rect =
            let (Vect x y) = b.pos
            in { x: x, y: y, w: b.width, h: b.height }