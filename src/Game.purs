module Game 
  ( Game
  , Event (..)
  , init
  , update
  , view
  ) where

import Prelude

import Anim (Anim, Speed, animate, current, getX, getY, isMoving, moveTo)
import Block (Block)
import Block as Block
import Control.Monad.Eff (Eff)
import Data.Time.Duration (Milliseconds)
import Graphics.Canvas (CANVAS, Context2D, clearRect)
import Vect (Vect(..))

type Game =
  { block :: Anim Block
  }


data Event 
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | Ticked Milliseconds
  

init :: Game
init =
  { block: Block.create "blue" 1.0 1.0 2 (Vect 0.0 0.0)
  }


speed :: Speed
speed = 10.0 / 1000.0


update :: forall eff . Event -> Game -> Eff eff Game
update MoveRight game
  | isMoving game.block = pure game
  | otherwise =
    pure $ game { block = moveTo speed (Vect 3.0 (getY $ current game.block)) game.block }
update MoveLeft game
  | isMoving game.block = pure game
  | otherwise =
    pure $ game { block = moveTo speed (Vect 0.0 (getY $ current game.block)) game.block }
update MoveUp game
  | isMoving game.block = pure game
  | otherwise =
    pure $ game { block = moveTo speed (Vect (getX $ current game.block) 0.0) game.block }
update MoveDown game
  | isMoving game.block = pure game
  | otherwise =
    pure $ game { block = moveTo speed (Vect (getX $ current game.block) 3.0) game.block }
update (Ticked delta) game = 
  pure $ game { block = animate delta game.block }


view :: forall eff . Context2D -> Game -> Eff ( canvas :: CANVAS | eff ) Unit
view ctx game = do
  void $ clearRect ctx { x: 0.0, y: 0.0, w: 4.0, h: 4.0 }
  Block.draw ctx (current game.block)