module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Char (toCharCode)
import Data.Maybe (Maybe(Just))
import Data.NonEmpty (NonEmpty(..), foldl1)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst)
import Game as Game
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, ScaleTransform, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, scale)
import JQuery (setScore, showGameOver)
import Math (abs)
import Partial.Unsafe (unsafePartial)
import Signal (foldp, merge, runSignal, (~>))
import Signal.DOM (animationFrame, keyPressed)
import SignalExt (foldEff)


-- | this is the entry point into the app
-- | we setup the canvas, settings and start the game loop
main :: ∀ e. Eff ( canvas :: CANVAS, timer :: TIMER, dom :: DOM, random :: RANDOM | e) Unit
main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    scaling <- calculateScaling canvas
    _ <- scale scaling ctx

    runGameLoop ctx 

-- | we are using `Signal` to map `animationFrame` and keyboard signals into
-- | a `GameState`-signal using `Game.update`
-- | finally we run this signal using `view`
runGameLoop :: ∀ e. Context2D -> Eff (timer :: TIMER, canvas :: CANVAS, random :: RANDOM, dom :: DOM | e) Unit
runGameLoop ctx = do
  gameSignal <- mkGameSignal
  runSignal $ view <$> gameSignal

  where
    view game = do
      Game.view ctx game
      showGameOver game.gameOver
      setScore $ show game.score

    mkGameSignal = do
      sigs <- signals
      initBoard <- Game.init
      foldEff Game.update initBoard sigs

    signals = do
      leftSignal  <- (\s -> s ~> onDown (Game.Move Game.Left))  <$> keyPressed 37 
      upSignal    <- (\s -> s ~> onDown (Game.Move Game.Up))    <$> keyPressed 38
      rightSignal <- (\s -> s ~> onDown (Game.Move Game.Right)) <$> keyPressed 39
      downSignal  <- (\s -> s ~> onDown (Game.Move Game.Down))  <$> keyPressed 40
      resetSignal <- (\s -> s ~> onDown Game.Reset)             <$> keyPressed (toCharCode 'R')
      tickSignal  <- mkTickSignal
      pure $ foldl1 merge (NonEmpty tickSignal [leftSignal, upSignal, rightSignal, downSignal, resetSignal])

    mkTickSignal = do
      animFrameSignal <- animationFrame
      pure $ foldp (\ n (Tuple _ l) -> Tuple (abs $ n-l) n) (Tuple 0.0 0.0) animFrameSignal 
        ~> Game.Ticked <<< Milliseconds <<< fst

    onDown msg true  = msg
    onDown _   false = Game.Ticked $ Milliseconds 0.0



-- calculates a ScaleTransform so we can use 1pixel per block
calculateScaling :: ∀ e. CanvasElement         
        -> Eff ( canvas :: CANVAS | e) ScaleTransform            
calculateScaling canvas = do
  cW <- getCanvasWidth canvas
  cH <- getCanvasHeight canvas
  pure { scaleX: cW / 4.0, scaleY: cH / 4.0 }