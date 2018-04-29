module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (replicate)
import Game (Cell(..))
import Game as Game
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff . Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | eff ) Unit
main = runTest do
  suite "game logic tests" do
    test "arithmetic" do
      Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
      Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
      Assert.equal 4 (2 + 2)
      Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal 5 (2 + 2)
  suite "Game mechanics" $ do
    suite "stackRows Single rows merging matching" $ do
      test "fills up no cells to 4 empty cells" do
        let expected = Game.Row (replicate 4 Game.Empty) :: Game.Row Int
            actual   = Game.stackRow (==) (Game.Row [] :: Game.Row Int)
        Assert.equal expected actual
      test "fills up one single cell to 4 cells" do
        let expected = Game.Row (replicate 3 Game.Empty <> [Single 1]) :: Game.Row Int
            actual   = Game.stackRow (==) (Game.Row [Single 1] :: Game.Row Int)
        Assert.equal expected actual
      test "fills up one single cell in the middle to 4 cells with the filled right" do
        let expected = Game.Row (replicate 3 Game.Empty <> [Single 1]) :: Game.Row Int
            actual   = Game.stackRow (==) (Game.Row [Empty, Single 1, Empty, Empty] :: Game.Row Int)
        Assert.equal expected actual
      test "moves non-matching singles to the right" do
        let expected = Game.Row (replicate 2 Game.Empty <> [Single 1, Single 2]) :: Game.Row Int
            actual   = Game.stackRow (==) (Game.Row [Empty, Single 1, Single 2, Empty] :: Game.Row Int)
        Assert.equal expected actual
      test "moves matching singles to the right merging them to Double" do
        let expected = Game.Row (replicate 3 Game.Empty <> [Double 2 2]) :: Game.Row Int
            actual   = Game.stackRow (==) (Game.Row [Empty, Single 2, Single 2, Empty] :: Game.Row Int)
        Assert.equal expected actual
      test "moves matching singles with empty in between to the right merging them to Double" do
        let expected = Game.Row (replicate 3 Game.Empty <> [Double 2 2]) :: Game.Row Int
            actual   = Game.stackRow (==) (Game.Row [Single 2, Empty, Empty, Single 2] :: Game.Row Int)
        Assert.equal expected actual
      test "merges two matching to the right" do
        let expected = Game.Row (replicate 2 Game.Empty <> [Double 1 1, Double 2 2]) :: Game.Row Int
            actual   = Game.stackRow (==) (Game.Row [Single 1, Single 1, Single 2, Single 2] :: Game.Row Int)
        Assert.equal expected actual
      test "merges matching singles in the middle" do
        let expected = Game.Row ([Empty, Single 1, Double 2 2, Single 1]) :: Game.Row Int
            actual   = Game.stackRow (==) (Game.Row [Single 1, Single 2, Single 2, Single 1] :: Game.Row Int)
        Assert.equal expected actual


