module Game 
  ( Game
  , Event (..)
  , init
  , update
  , view
  , Row (..), Cell (..)
  , stackRow
  ) where

import Prelude

import Anim (Anim, Speed, animate, current, getX, getY, isMoving, moveTo)
import Block (Block)
import Block as Block
import Control.Monad.Eff (Eff)
import Data.Array (foldr, length, replicate, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
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

----------------------------------------------------------------------
-- representation of the board

data Board a =
  Board (Array (Row a))


data Row a 
  = Row (Array (Cell a))

derive instance eqRow :: Eq a => Eq (Row a)
instance showRow :: Show a => Show (Row a) where
  show (Row rs) = "<" <> joinWith ", " (map show rs) <> ">"


data Cell a
  = Empty
  | Single a
  | Double a a


derive instance eqCell :: Eq a => Eq (Cell a)
instance showCell :: Show a => Show (Cell a) where
  show Empty = "[]"
  show (Single a) = "[" <> show a <> "]"
  show (Double a1 a2) = "[" <> show a1 <> ", " <> show a2 <> "]"


stackRow :: forall a . (a -> a -> Boolean) -> Row a -> Row a
stackRow eqVal (Row cs) = 
  Row $ fillUp $ stackLast $ foldr stackCells (Tuple mempty Nothing) cs
  where 
    stackCells left (Tuple tail right) =
      case Tuple (cellVal left) right of
        Tuple (Just v1) (Just v2)
          | eqVal v1 v2         -> Tuple (Double v1 v2 : tail) Nothing
          | otherwise           -> Tuple (Single v2 : tail) (Just v1)
        Tuple Nothing (Just v2) -> Tuple tail right
        Tuple (Just v1) Nothing -> Tuple tail (Just v1)
        Tuple Nothing Nothing   -> Tuple tail Nothing
    stackLast (Tuple tail Nothing)  = tail
    stackLast (Tuple tail (Just v)) = Single v : tail
    fillUp rs = replicate (4 - length rs) Empty <> rs
    cellVal Empty = Nothing
    cellVal (Single a) = Just a
    cellVal (Double a _) = Just a


  