module Game 
  ( Game
  , Event (..)
  , init
  , update
  , view
  , Board, Row (..), Cell (..)
  , stackRow
  ) where

import Prelude

import Anim (class Animable, class HasPos, Anim, Speed, animate, current, isMoving, moveTo)
import Block (Block)
import Block as Block
import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Data.Array (any, foldr, length, mapWithIndex, replicate, uncons, zipWith, (:))
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS, Context2D, clearRect)
import Vect (Vect(..))


type Game =
  { board :: Board (Anim Block)
  }


data Event 
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | Ticked Milliseconds
  
init :: Game
init =
  { board: 
      emptyBoard 
      # insertCell 1 1 (Block.create "blue" 1.0 1.0 2 (Vect 1.0 1.0))
      # insertCell 2 2 (Block.create "red" 1.0 1.0 2 (Vect 2.0 2.0))
  }


speed :: Speed
speed = 10.0 / 1000.0


update :: ∀ eff . Event -> Game -> Eff eff Game
update MoveRight game
  | isAnimating game.board = pure game
  | otherwise =
    let board' =
          stackRight (\ab1 ab2 -> Block.sameValue (current ab1) (current ab2)) game.board
          # moveBoardCells speed
    in pure $ game { board = board' }
update MoveLeft game
  | isAnimating game.board = pure game
  | otherwise =
    let board' =
          stackLeft (\ab1 ab2 -> Block.sameValue (current ab1) (current ab2)) game.board
          # moveBoardCells speed
    in pure $ game { board = board' }
update MoveUp game
  | isAnimating game.board = pure game
  | otherwise =
    let board' =
          stackTop (\ab1 ab2 -> Block.sameValue (current ab1) (current ab2)) game.board
          # moveBoardCells speed
    in pure $ game { board = board' }
update MoveDown game
  | isAnimating game.board = pure game
  | otherwise =
    let board' =
          stackBottom (\ab1 ab2 -> Block.sameValue (current ab1) (current ab2)) game.board
          # moveBoardCells speed
    in pure $ game { board = board' }
update (Ticked delta) game = 
  pure $ game { board = mergeBlocks $ animateBoard delta game.board }


view :: ∀ eff . Context2D -> Game -> Eff ( canvas :: CANVAS | eff ) Unit
view ctx game = do
  void $ clearRect ctx { x: 0.0, y: 0.0, w: 4.0, h: 4.0 }
  viewBoard (\ctx animBlock -> Block.draw ctx (current animBlock)) game.board ctx

----------------------------------------------------------------------
-- representation of the board

data Board a =
  Board (Array (Row a))

derive instance functorBoard :: Functor Board


emptyBoard :: ∀ a . Board a
emptyBoard = 
  Board (replicate 4 emptyRow)
  where
    emptyRow = Row (replicate 4 Empty)


insertCell :: ∀ a . Int -> Int -> a -> Board a -> Board a
insertCell inRow inCol val (Board rows) =
  Board $ mapWithIndex insertRow rows
  where
    insertRow row (Row cells) =
      Row $ mapWithIndex (insertCell row) cells
    insertCell row col cell
      | row == inRow && col == inCol = Single val
      | otherwise                    = cell


animateBoard :: ∀ a . Animable a => Milliseconds -> Board a -> Board a
animateBoard delta = map (animate delta)


mergeBlocks :: Board (Anim Block) -> Board (Anim Block)
mergeBlocks board@(Board rows)
  | isAnimating board = board
  | otherwise         = Board $ map (mergeRow $ lift2 Block.merge) rows


viewBoard :: ∀ eff a . (Context2D -> a -> Eff ( canvas :: CANVAS | eff) Unit) -> Board a -> Context2D -> Eff ( canvas :: CANVAS | eff) Unit
viewBoard viewVal (Board rows) ctx =
  traverse_ viewRow rows
  where
    viewRow (Row cells) =
      traverse_ viewCol cells
    viewCol Empty =
      pure unit
    viewCol (Single val) =
      viewVal ctx val
    viewCol (Double val1 val2) = do
      viewVal ctx val1
      viewVal ctx val2


isAnimating :: ∀ a . Board (Anim a) -> Boolean
isAnimating (Board rows) =
  any isRowAnimating rows
  where
    isRowAnimating (Row cells) =
      any isCellAnimating cells
    isCellAnimating Empty = false
    isCellAnimating (Single v) = isMoving v
    isCellAnimating (Double v1 v2) = isMoving v1 || isMoving v2


moveBoardCells :: ∀ eff a . HasPos a =>  Speed -> Board (Anim a) -> Board (Anim a)
moveBoardCells speed (Board rows) =
  Board $ mapWithIndex moveRow rows
  where
    moveRow i (Row cells) =
      Row $ mapWithIndex (moveCol i) cells
    moveCol _ _ Empty =
      Empty
    moveCol rowNr colNr (Single val) =
      Single (moveTo speed (Vect (toNumber colNr) (toNumber rowNr)) val)
    moveCol rowNr colNr (Double val1 val2) =
      let mov1 = moveTo speed (Vect (toNumber colNr) (toNumber rowNr)) val1
          mov2 = moveTo speed (Vect (toNumber colNr) (toNumber rowNr)) val2
      in Double mov1 mov2


stackRight :: ∀ a . (a -> a -> Boolean) -> Board a -> Board a
stackRight eqVal (Board rows) =
  Board $ map (stackRow eqVal) rows


stackLeft :: ∀ a . (a -> a -> Boolean) -> Board a -> Board a
stackLeft eqVal = reverse >>> stackRight eqVal >>> reverse


stackBottom :: ∀ a . (a -> a -> Boolean) -> Board a -> Board a
stackBottom eqVal = transpose >>> stackRight eqVal >>> transpose


stackTop :: ∀ a . (a -> a -> Boolean) -> Board a -> Board a
stackTop eqVal = transpose >>> reverse >>> stackRight eqVal >>> reverse >>> transpose


reverse :: ∀ a . Board a -> Board a
reverse (Board rows) =
  Board $ map reverseRow rows
  where
    reverseRow (Row cells) =
      Row $ Array.reverse cells

transpose :: ∀ a . Board a -> Board a
transpose (Board rows) = 
  Board $ case uncons rows of 
    Nothing                   -> replicate 4 (Row [])
    Just { head: r, tail: rs} -> zipRowsWith (:) r (transpose (Board rs))
  where
    zipRowsWith op (Row cs) (Board rs) = 
      zipWith (\c (Row cs) -> Row (c `op` cs)) cs rs


data Row a 
  = Row (Array (Cell a))

derive instance eqRow :: Eq a => Eq (Row a)
instance showRow :: Show a => Show (Row a) where
  show (Row rs) = "<" <> joinWith ", " (map show rs) <> ">"

derive instance functorRow :: Functor Row   


data Cell a
  = Empty
  | Single a
  | Double a a

derive instance eqCell :: Eq a => Eq (Cell a)
derive instance functorCell :: Functor Cell
instance showCell :: Show a => Show (Cell a) where
  show Empty = "[]"
  show (Single a) = "[" <> show a <> "]"
  show (Double a1 a2) = "[" <> show a1 <> ", " <> show a2 <> "]"


stackRow :: ∀ a . (a -> a -> Boolean) -> Row a -> Row a
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


mergeRow :: ∀ a . (a -> a -> a) -> Row a -> Row a
mergeRow merge (Row cs) = Row $ map mergeCell cs
  where
    mergeCell Empty = Empty
    mergeCell (Single a) = Single a
    mergeCell (Double a1 a2) = Single (merge a1 a2)