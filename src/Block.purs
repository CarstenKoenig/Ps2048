module Block where

import Prelude

import Anim (class HasPos, Anim, Pos, initStatic)
import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS, Context2D, TextAlign(..), beginPath, closePath, fillRect, fillText, measureText, setFillStyle, setFont, setTextAlign)
import Vect (Vect(..))

type Color = String

newtype Block = Block
    { width :: Number
    , height :: Number
    , value :: Int
    , pos :: Pos
    }


create :: Number -> Number -> Int -> Pos -> Anim Block
create wdt hgt val p =
    initStatic $ Block
        { width: wdt
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


merge :: Block -> Block -> Block
merge (Block b1) (Block b2) =
    Block $ b1 { value = b1.value + b2.value }    


draw :: forall eff . Context2D -> Block -> Eff ( canvas :: CANVAS | eff ) Unit
draw ctx (Block b) = do
    void $ beginPath ctx
    void $ setFillStyle (color b.value) ctx
    void $ fillRect ctx rect
    void $ closePath ctx

    -- Text
    let text = show b.value
    void $ setFont "10px Arial" ctx
    textWidth <- _.width <$> measureText ctx text
    let adjusted = min 0.8 ( 8.0 / textWidth )
    void $ setFont (show adjusted <> "px Arial") ctx
    void $ setFillStyle "white" ctx
    textHeight <- _.width <$> measureText ctx "M"
    void $ setTextAlign ctx AlignCenter
    void $ fillText ctx text (x + 0.5) (y + 0.45 + textHeight / 2.0)
    where
        (Vect x y) = b.pos        
        rect = { x: x, y: y, w: b.width, h: b.height }
        -- found the colors here: https://github.com/pclucas14/2048-Game/blob/master/colours.py
        color 2    = "rgb(244,67,54)"
        color 4    = "rgb(234,30,99)"
        color 8    = "rgb(156,39,156)"
        color 16   = "rgb(103,58,183)"
        color 32   = "rgb(33,150,243)"
        color 64   = "rgb(0,150,136)"
        color 128  = "rgb(139,195,74)"
        color 256  = "rgb(60,175,80)"
        color 512  = "rgb(255,152,0)"
        color 1024 = "rgb(255,87,34)"
        color 2048 = "rgb(121,85,72)"
        color _ = "black"