module Block where

import Prelude

import Anim (class Animable, Anim, animate, current, isRunning)
import AnimPos (Pos, Speed, moveTo)
import Control.Monad.Eff (Eff)
import Data.Int (toNumber)
import DrawParams (Settings)
import Graphics.Canvas (CANVAS, Context2D, TextAlign(..), beginPath, closePath, fillRect, fillText, measureText, setFillStyle, setFont, setTextAlign)
import Vect (Vect(..))

type Color = String

newtype Block = Block
    { value  :: Int
    , pos    :: Anim Pos
    }


create :: Settings -> Int -> Int -> Int -> Block
create params val x y =
    Block
        { value: val
        , pos: pure (Vect (toNumber x * params.blockWidth) (toNumber y * params.blockHeight))
        }


instance blockAnimable :: Animable Block where
    animate delta (Block b) = Block $ b { pos = animate delta b.pos }
    isRunning (Block b)     = isRunning b.pos


value :: Block -> Int
value (Block b) = b.value


sameValue :: Block -> Block -> Boolean
sameValue (Block b1) (Block b2) = 
    b1.value == b2.value


merge :: Block -> Block -> Block
merge (Block b1) (Block b2) =
    Block $ b1 { value = b1.value + b2.value }    


move :: Settings -> Speed -> Int -> Int -> Block -> Block
move params speed col row (Block b) =
    Block $ b { pos = moveTo speed toPos b.pos }
    where
        toPos = Vect 
            (toNumber col * params.blockWidth) 
            (toNumber row * params.blockHeight)

draw :: forall eff . Settings -> Context2D -> Block -> Eff ( canvas :: CANVAS | eff ) Unit
draw params ctx (Block b) = do
    void $ beginPath ctx
    void $ setFillStyle (color b.value) ctx
    void $ fillRect ctx rect
    void $ closePath ctx

    -- Text
    let text = show b.value
    void $ setFont "10px Arial" ctx
    textWidth <- _.width <$> measureText ctx text
    let adjusted = 6.0 * params.blockWidth / textWidth
    void $ setFont (show adjusted <> "px Arial") ctx
    void $ setFillStyle "white" ctx
    textHeight <- _.width <$> measureText ctx "M"
    void $ setTextAlign ctx AlignCenter
    void $ fillText ctx text (x + params.blockWidth / 2.0) (y + params.blockWidth / 2.0 + textHeight / 2.4)
    where
        (Vect x y) = current b.pos        
        rect = { x: x, y: y, w: params.blockWidth, h: params.blockHeight }
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