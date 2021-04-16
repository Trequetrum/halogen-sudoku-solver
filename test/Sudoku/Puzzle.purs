module Test.Sudoku.Puzzle where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Sudoku.Puzzle (blankMetaBoard, fromString)
import Test.Basic.Data (startingBoard, startingBoardString)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Sudoku Puzzle" do
    describe "Smart Constructors" do
      it "Parse a Puzzle" $ fromString startingBoardString `shouldEqual` 
        (Right $ Tuple blankMetaBoard startingBoard)