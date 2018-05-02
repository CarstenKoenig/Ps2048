module Game 
  ( Game
  , Event (..)
  , Direction (..)
  , init
  , update
  , view
  , Board, Row (..), Cell (..)
  , stackRow
  ) where

import Prelude

import Anim (class Animable, animate, isRunning)
import AnimPos (Speed)
import Block (Block)
import Block as Block
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt, randomRange)
import Data.Array (any, concat, concatMap, filter, foldMap, foldl, foldr, index, length, mapWithIndex, replicate, uncons, zipWith, (:))
import Data.Array as Array
import Data.Foldable (class Foldable, and, sum, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..), fst, snd)
import DrawParams (Settings)
import Graphics.Canvas (CANVAS, Context2D, clearRect)
import Type.Data.Boolean (kind Boolean)


type Game =
  { params :: Settings
  , board :: Board Block
  , animationRunning :: Boolean -- used to trigger merge/insert after animation ended
  , gameOver :: Boolean
  , score :: Int
  }


data Event 
  = Move Direction
  | Ticked Milliseconds
  | Reset


data Direction
  = Left
  | Right
  | Up
  | Down


init :: forall eff . Settings -> Eff ( random :: RANDOM | eff ) Game
init params = do
  board <- randomBoard params
  pure 
    { params: params
    , board: board 
    , animationRunning: false
    , gameOver : false
    , score : 0
    }


speed :: Speed
speed = 500.0 / 1000.0


update :: ∀ eff . Event -> Game -> Eff ( random :: RANDOM |  eff) Game
update (Move dir) game
  | not (isRunning game.board) && isValidMove eqAnimBlock dir game.board =
    let board' =
          stackMove eqAnimBlock dir game.board
          # moveBoardCells game.params speed
    in pure $ game 
      { board = board' 
      , animationRunning = true
      , score = game.score + mergeScore board'
      }
  | otherwise = pure game
update (Ticked delta) game
  | isRunning game.board =
    pure $ game { board = animate delta game.board }
  | game.animationRunning = do
    board' <- insertRandom game.params (mergeBlocks game.board)
    case board' of
      Nothing ->
        pure $ game { animationRunning = false, gameOver = true }
      Just board'' ->
        pure $ game
          { board = board''
          , animationRunning = false
          , gameOver = not $ any (\mv -> isValidMove eqAnimBlock mv board'') [Left, Right, Up, Down]
          }
  | otherwise = pure game
update Reset game = init game.params


eqAnimBlock :: Block -> Block -> Boolean
eqAnimBlock = Block.sameValue


view :: ∀ eff . Context2D -> Game -> Eff ( canvas :: CANVAS | eff ) Unit
view ctx game = do
  void $ clearRect ctx { x: 0.0, y: 0.0, w: game.params.canvasWidth, h: game.params.canvasHeight }
  viewBoard (Block.draw game.params) game.board ctx

----------------------------------------------------------------------
-- representation of the board

data Board a =
  Board (Array (Row a))

derive instance functorBoard :: Functor Board

instance foldableBoard :: Foldable Board where
  foldMap inj board = foldMap inj (cellValues board)
  foldr f s board = foldr f s (cellValues board)
  foldl f s board = foldl f s (cellValues board)


cellValues :: forall a . Board a -> Array a
cellValues (Board rows) = concatMap getVals rows  
  where getVals (Row cells) = concatMap getCell cells
        getCell Empty = []
        getCell (Single v) = [v]
        getCell (Double v1 v2) = [v1, v2]


instance boardAnimable :: Animable a => Animable (Board a) where
  animate delta = map (animate delta)
  isRunning = any isRunning


randomBoard :: forall eff . Settings -> Eff ( random :: RANDOM | eff ) (Board Block)
randomBoard params = do
  let board = emptyBoard
  board' <- fromMaybe board <$> insertRandom params board
  fromMaybe board' <$> insertRandom params board'


emptyBoard :: ∀ a . Board a
emptyBoard = 
  Board (replicate 4 emptyRow)
  where
    emptyRow = Row (replicate 4 Empty)


insertRandom :: forall eff . Settings -> Board Block -> Eff ( random :: RANDOM | eff ) (Maybe (Board Block))
insertRandom params board@(Board rows) = do
  ws4 <- randomRange 0.0 1.0
  let val = if ws4 > 0.66 then 4 else 2
  let freePoss = freePositions board
  let freeCount = length freePoss
  if freeCount <= 0 then pure Nothing else do
    ind <- randomInt 0 (freeCount - 1)
    case index freePoss ind of
      Just (Tuple row col) -> pure $ Just $
        insertCell row col (Block.create params val col row) board
      Nothing -> pure Nothing


freePositions :: Board Block -> Array (Tuple Int Int)
freePositions (Board rows) =
  mapWithIndex emptyCells rows
  # concat
  # filter snd
  # map fst
  where
    emptyCells i (Row cells) = mapWithIndex (emptyCell i) cells
    emptyCell i j Empty = Tuple (Tuple i j) true
    emptyCell i j _     = Tuple (Tuple i j) false


insertCell :: ∀ a . Int -> Int -> a -> Board a -> Board a
insertCell inRow inCol val (Board rows) =
  Board $ mapWithIndex insertRow rows
  where
    insertRow row r@(Row cells) 
      | row == inRow = Row $ mapWithIndex insertCell' cells
      | otherwise    = r
    insertCell' col cell
      | col == inCol = Single val
      | otherwise    = cell


mergeBlocks :: Board Block -> Board Block
mergeBlocks board@(Board rows)
  | isRunning board = board
  | otherwise         = Board $ map (mergeRow Block.merge) rows


mergeScore :: Board Block -> Int
mergeScore board@(Board rows) =
    sum $ map mergeScoreRow rows
    where 
      mergeScoreRow (Row cells) =
        sum $ map mergeScoreCell cells
      mergeScoreCell Empty = 0
      mergeScoreCell (Single _) = 0
      mergeScoreCell (Double a _) = Block.value a


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



moveBoardCells :: Settings -> Speed -> Board Block -> Board Block
moveBoardCells params sp (Board rows) =
  Board $ mapWithIndex moveRow rows
  where
    moveRow i (Row cells) =
      Row $ mapWithIndex (moveCol i) cells
    moveCol _ _ Empty =
      Empty
    moveCol rowNr colNr (Single val) =
      Single (Block.move params sp colNr rowNr val)
    moveCol rowNr colNr (Double val1 val2) =
      let mov1 = Block.move params sp colNr rowNr val1
          mov2 = Block.move params sp colNr rowNr val2
      in Double mov1 mov2


isValidMove :: forall a . (a -> a -> Boolean) -> Direction -> Board a -> Boolean
isValidMove eqVal dir board@(Board rows) =
  let board'@(Board rows') = stackMove eqVal dir board
  in not $ and $ zipWith sameRow rows rows'
  where
    sameRow (Row cells) (Row cells') = 
      and $ zipWith sameCell cells cells'
    sameCell Empty Empty = true
    sameCell (Single v) (Single v') = eqVal v v'
    sameCell (Double v1 v2) (Double v1' v2') = eqVal v1 v1' && eqVal v2 v2'
    sameCell _ _ = false


stackMove :: forall a . (a -> a -> Boolean) -> Direction -> Board a -> Board a
stackMove eqVal Left  = stackLeft eqVal
stackMove eqVal Right = stackRight eqVal
stackMove eqVal Up    = stackTop eqVal
stackMove eqVal Down  = stackBottom eqVal


stackRight :: forall a . (a -> a -> Boolean) -> Board a -> Board a
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
      zipWith (\c' (Row cs') -> Row (c' `op` cs')) cs rs


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