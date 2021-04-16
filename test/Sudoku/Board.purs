module Test.Sudoku.Board where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Safe.Coerce (coerce)
import Sudoku.Board (allCellsValid, batchDropOptions, fromCells, isSolved, isSolvedOrInvalid, modifyAtIndex, noForcedPeerDuplicates, unconstrainedBoard)
import Sudoku.Board as Brd
import Sudoku.Cell (dropOptions, isForced)
import Sudoku.Cell.Internal (Cell(..))
import Sudoku.Index (boundedIndex)
import Test.Basic.Data (badCellboard1, badCellboard2, forcedCellDuplicateBoard, forcedCellDuplicateBoard2, startingBoard, startingBoardSolved, startingBoardString)
import Test.Spec (Spec, describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)


spec :: Spec Unit
spec =
  describe "Sudoku Board" do
    describe "Smart Constructors" do

      it "Parse Correct Board" $ Brd.fromString startingBoardString
        `shouldEqual` (Right startingBoard)
      it "Parse Correct Board - dirty" $ Brd.fromString """
        4.....8.5|.3.......|...7.....
        abcdefghijklmnopqrstuvwxyz`!@
        .2.....6.|....8.4..|....1....
        $%^&*()_+-
        ...6.3.7.|5..2.....|1.4......
        """ `shouldEqual` (Right $ 
        fromMaybe unconstrainedBoard $ fromCells $ coerce
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
      it "Parse Board Input String Too Short" $ Brd.fromString "123" `shouldSatisfy` isLeft 
      it "Parse Board Input String Too Long" $ Brd.fromString """
        0123456789 0123456789 0123456789 0123456789
        0123456789 0123456789 0123456789 0123456789
        0123456789
        """ `shouldSatisfy` isLeft

    describe "Predicates" do

      it "allCellsValid True" $ allCellsValid startingBoard `shouldEqual` true
      it "allCellsValid false Low" $ allCellsValid badCellboard1 `shouldEqual` false
      it "allCellsValid false High" $ allCellsValid badCellboard2 `shouldEqual` false
      it "isForcedCell True" $ isForced (Cell 256) `shouldEqual` true
      it "isForcedCell False OOB" $ isForced (Cell 512) `shouldEqual` false
      it "isForcedCell False" $ isForced (Cell 317) `shouldEqual` false
      it "noForcedPeerDuplicates True" $ noForcedPeerDuplicates startingBoard `shouldEqual` true
      it "noForcedPeerDuplicates False" $ noForcedPeerDuplicates forcedCellDuplicateBoard `shouldEqual` false
      it "noForcedPeerDuplicates False 2" $ noForcedPeerDuplicates forcedCellDuplicateBoard2 `shouldEqual` false
      it "isValid 1" $ Brd.isValid startingBoard `shouldEqual` true
      it "isValid 2" $ Brd.isValid badCellboard1 `shouldEqual` false
      it "isValid 3" $ Brd.isValid forcedCellDuplicateBoard2 `shouldEqual` false
      it "isSolved 1" $ isSolved startingBoardSolved `shouldEqual` true
      it "isSolved 2" $ isSolved startingBoard `shouldEqual` false
      it "isSolvedOrInvalid 1" $ isSolvedOrInvalid startingBoardSolved `shouldEqual` true
      it "isSolvedOrInvalid 2" $ isSolvedOrInvalid badCellboard1 `shouldEqual` true
      it "isSolvedOrInvalid 3" $ isSolvedOrInvalid forcedCellDuplicateBoard2 `shouldEqual` true
      it "isSolvedOrInvalid 4" $ isSolvedOrInvalid startingBoard `shouldEqual` false

    describe "Update Board State" do
      it "batchDropOptions" $ batchDropOptions 
        [Tuple (boundedIndex 0) (Cell 511), Tuple (boundedIndex 2) (Cell 503), Tuple (boundedIndex 8) (Cell 503)] 
          startingBoard `shouldEqual` ( modifyAtIndex (dropOptions $ Cell 511) (boundedIndex 0) $
          fromMaybe unconstrainedBoard $ fromCells $ coerce
          [ 1,511,8,32,64,2,511,511,8
          , 16,511,511,128,511,511,511,256,32
          , 511,32,4,511,8,511,511,511,128
          , 4,128,2,1,511,511,256,32,511
          , 8,64,16,511,511,511,1,511,511
          , 256,1,511,2,511,8,16,511,511
          , 511,511,511,511,511,511,511,2,256
          , 511,511,1,511,511,511,64,8,4
          , 2,511,511,511,32,4,128,511,1
          ])