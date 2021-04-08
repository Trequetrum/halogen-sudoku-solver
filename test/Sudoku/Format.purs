module Test.Sudoku.Format where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Safe.Coerce (coerce)
import Sudoku.Common (Cell(..))
import Sudoku.Format (parseBoard, parsePuzzle)
import Test.Basic.Data (startingBoard, startingBoardString)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

eitherIsLeft :: forall a b. Either a b -> Boolean
eitherIsLeft (Left _) = true
eitherIsLeft _ = false

spec :: Spec Unit
spec =
  describe "Sudoku Boardsize 9 - Format" do

-------------------------------------------------------------------
-- Format
-------------------------------------------------------------------

--  it "makeStarterCell 0" $ C.makeStarterCell 0 `shouldEqual 511
--  it "makeStarterCell 1" $ C.makeStarterCell 1 `shouldEqual 1
--  it "makeStarterCell 9" $ C.makeStarterCell 9 `shouldEqual 256
--  it "makeStarterCell 10" $ C.makeStarterCell 10 `shouldEqual 511
    it "Parse Correct Board" $ parseBoard startingBoardString
      `shouldEqual` (Right startingBoard)
    it "Parse Correct Board - dirty" $ parseBoard """
      4.....8.5|.3.......|...7.....
      abcdefghijklmnopqrstuvwxyz`!@
      .2.....6.|....8.4..|....1....
      $%^&*()_+-
      ...6.3.7.|5..2.....|1.4......
      """ `shouldEqual` (Right $ coerce
      [ 8,511,511,511,511,511,128,511,16
      , 511,4,511,511,511,511,511,511,511
      , 511,511,511,64,511,511,511,511,511
      , 511,2,511,511,511,511,511,32,511
      , 511,511,511,511,128,511,8,511,511
      , 511,511,511,511,1,511,511,511,511
      , 511,511,511,32,511,4,511,64,511
      , 16,511,511,2,511,511,511,511,511
      , 1,511,8,511,511,511,511,511,511
      ])
    it "Parse Board Input String Too Short" $ parseBoard "123" `shouldSatisfy` eitherIsLeft
    it "Parse Board Input String Too Long" $ parseBoard """
      0123456789 0123456789 0123456789 0123456789
      0123456789 0123456789 0123456789 0123456789
      0123456789
      """ `shouldSatisfy` eitherIsLeft
    it "Parse a Puzzle" $ parsePuzzle startingBoardString `shouldEqual` 
      (Right $ Tuple { metaBoard: [], metaData: {} } startingBoard)