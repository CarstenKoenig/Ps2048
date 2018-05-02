module Block where

import Prelude

import Anim (class Animable, Anim, animate, current, isRunning)
import AnimColor (Color)
import AnimColor as Color
import AnimPos (Pos, moveTo)
import Control.Monad.Eff (Eff)
import Data.Time.Duration (Milliseconds(..))
import DrawParams (Settings)
import Graphics.Canvas (CANVAS, Context2D, TextAlign(..), beginPath, closePath, fillRect, fillText, measureText, setFillStyle, setFont, setTextAlign)
import Vect (Vect(..))


newtype Block = Block
    { value  :: Int
    , pos    :: Anim Pos
    , color  :: Anim Color
    }


create :: Settings -> Int -> Int -> Int -> Block
create params val x y =
    Block
        { value: val
        , pos: pure (params.toPos x y)
        , color: pure (valueToColor val)
        }


instance blockAnimable :: Animable Block where
    animate delta (Block b) = 
        Block $ b 
            { pos = animate delta b.pos 
            , color = animate delta b.color
            }
    isRunning (Block b) = 
        isRunning b.pos || isRunning b.color


value :: Block -> Int
value (Block b) = b.value


sameValue :: Block -> Block -> Boolean
sameValue (Block b1) (Block b2) = 
    b1.value == b2.value


move :: Settings -> Int -> Int -> Int -> Block -> Block
move params col row value (Block b) =
    Block $ b 
        { pos = moveTo params.speed toPos b.pos 
        , color = Color.changeColorTo (Milliseconds 300.0) (valueToColor value) b.color
        , value = value
        }
    where
        toPos = params.toPos col row


draw :: forall eff . Settings -> Context2D -> Block -> Eff ( canvas :: CANVAS | eff ) Unit
draw params ctx (Block b) = do
    void $ beginPath ctx
    void $ setFillStyle (Color.toCssColor $ current b.color) ctx
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
valueToColor :: Int -> Color
valueToColor 2    = Color.create 244 67 54
valueToColor 4    = Color.create 234 30 99
valueToColor 8    = Color.create 156 39 156
valueToColor 16   = Color.create 103 58 183
valueToColor 32   = Color.create 33 150 243
valueToColor 64   = Color.create 0 150 136
valueToColor 128  = Color.create 139 195 74
valueToColor 256  = Color.create 60 175 80
valueToColor 512  = Color.create 255 152 0
valueToColor 1024 = Color.create 255 87 34
valueToColor 2048 = Color.create 121 85 72
valueToColor _    = Color.create 0 0 0