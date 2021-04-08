module Test.Sudoku.Common where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Safe.Coerce (coerce)
import Sudoku.Common (Cell(..), Option(..), allCellsValid, allOptions, allOptionsInt, asOptions, basicGroup, batchDropOptions, boardRoot, boardSize, countOptions, dropOptions, firstOption, hasOption, indicesBox, indicesCol, indicesPeers, indicesRow, isForced, isSolved, isSolvedOrInvalid, isSuperset, isValid, noForcedPeerDuplicates, notDisjoint, toBox, toCell, toCol, toRow)
import Test.Basic.Data (badCellboard, badCellboard2, forcedCellDuplicateBoard, forcedCellDuplicateBoard2, startingBoard, startingBoardSolved)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


eitherIsLeft :: forall a b. Either a b -> Boolean
eitherIsLeft (Left _) = true
eitherIsLeft _ = false

spec :: Spec Unit
spec =
  describe "Sudoku Boardsize 9 - Common" do

-------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------

    describe "Invariants" do
      it "boardSize" $ boardSize `shouldEqual` 9
      it "boardRoot" $ boardRoot `shouldEqual` 3
      it "allOptions" $ allOptions `shouldEqual` coerce
        [1,2,3,4,5,6,7,8,9]
      it "allOptionsCell" $ allOptionsInt `shouldEqual` 511
      it "basicGroup" $ basicGroup `shouldEqual`
        [0,1,2,3,4,5,6,7,8]

-------------------------------------------------------------------
-- Binary Operations for Cells
-------------------------------------------------------------------

    describe "Binary Operations for Cells" do
      it "toCell 0" $ toCell (Option 0) `shouldEqual` Cell 0
      it "toCell 3" $ toCell (Option 3) `shouldEqual` Cell 4
      it "toCell 9" $ toCell (Option 9) `shouldEqual` Cell 256
      it "dropOptions 56 56" $ dropOptions (Cell 56) (Cell 56) `shouldEqual` Cell 0
      it "dropOptions 56 511" $ dropOptions (Cell 56) (Cell 511) `shouldEqual` Cell 455
      it "dropOptions 312 496" $ dropOptions (Cell 312) (Cell 496) `shouldEqual` Cell 192
      it "dropOptions 313 340" $ dropOptions (Cell 313) (Cell 340) `shouldEqual` Cell 68
      it "asOptions 147" $ asOptions (Cell 147) `shouldEqual` coerce [1,2,5,8]
      it "asOptions 286" $ asOptions (Cell 286) `shouldEqual` coerce [2,3,4,5,9]
      it "firstOption 147" $ firstOption (Cell 144) `shouldEqual` Option 5
      it "firstOption 286" $ firstOption (Cell 286) `shouldEqual` Option 2
      it "countOptions' 147" $ countOptions (Cell 147) `shouldEqual` 4
      it "countOptions' 286" $ countOptions (Cell 286) `shouldEqual` 5
      it "countOptions 147" $ countOptions (Cell 147) `shouldEqual` 4
      it "countOptions 286" $ countOptions (Cell 286) `shouldEqual` 5

-------------------------------------------------------------------
-- Common Sudoku Predicates
-------------------------------------------------------------------
      it "hasOption 1 1" $ hasOption (Option 1) (Cell 1) `shouldEqual` true
      it "hasOption 1 511" $ hasOption (Option 1) (Cell 511) `shouldEqual` true
      it "hasOption 5 511" $ hasOption (Option 5) (Cell 511) `shouldEqual` true
      it "hasOption 5 1" $ hasOption (Option 5) (Cell 1) `shouldEqual` false
      it "isSuperset 147 187" $ isSuperset (Cell 187) (Cell 147) `shouldEqual` true
      it "isSuperset 187 147" $ isSuperset (Cell 147) (Cell 187) `shouldEqual` false
      it "notDisjoint 21 16" $ notDisjoint (Cell 21) (Cell 16) `shouldEqual` true
      it "notDisjoint 21 10" $ notDisjoint (Cell 21) (Cell 10) `shouldEqual` false

      it "allCellsValid True" $ allCellsValid startingBoard `shouldEqual` true
      it "allCellsValid False High" $ allCellsValid badCellboard `shouldEqual` false
      it "allCellsValid False Low" $ allCellsValid badCellboard2 `shouldEqual` false
      it "isForcedCell True" $ isForced (Cell 256) `shouldEqual` true
      it "isForcedCell False OOB" $ isForced (Cell 512) `shouldEqual` false
      it "isForcedCell False" $ isForced (Cell 317) `shouldEqual` false
      it "noForcedPeerDuplicates True" $ noForcedPeerDuplicates startingBoard `shouldEqual` true
      it "noForcedPeerDuplicates False" $ noForcedPeerDuplicates forcedCellDuplicateBoard `shouldEqual` false
      it "noForcedPeerDuplicates False 2" $ noForcedPeerDuplicates forcedCellDuplicateBoard2 `shouldEqual` false
      it "isValid 1" $ isValid startingBoard `shouldEqual` true
      it "isValid 2" $ isValid badCellboard `shouldEqual` false
      it "isValid 3" $ isValid forcedCellDuplicateBoard2 `shouldEqual` false
      it "isSolved 1" $ isSolved startingBoardSolved `shouldEqual` true
      it "isSolved 2" $ isSolved startingBoard `shouldEqual` false
      it "isSolvedOrInvalid 1" $ isSolvedOrInvalid startingBoardSolved `shouldEqual` true
      it "isSolvedOrInvalid 2" $ isSolvedOrInvalid badCellboard `shouldEqual` true
      it "isSolvedOrInvalid 3" $ isSolvedOrInvalid forcedCellDuplicateBoard2 `shouldEqual` true
      it "isSolvedOrInvalid 4" $ isSolvedOrInvalid startingBoard `shouldEqual` false

-------------------------------------------------------------------
-- Index -> Row Column & Box
-------------------------------------------------------------------

    describe "Index -> Row Column & Box" do
      it "toCol 0" $ toCol 0 `shouldEqual` 0
      it "toCol 37" $ toCol 37 `shouldEqual` 1
      it "toCol 80" $ toCol 80 `shouldEqual` 8
      it "toRow 0" $ toRow 0 `shouldEqual` 0
      it "toRow 37" $ toRow 37 `shouldEqual` 4
      it "toRow 80" $ toRow 80 `shouldEqual` 8
      it "toBox 0" $ toBox 0 `shouldEqual` 0
      it "toBox 20" $ toBox 20 `shouldEqual` 0
      it "toBox 30" $ toBox 30 `shouldEqual` 4
      it "toBox 50" $ toBox 50 `shouldEqual` 4
      it "toBox 60" $ toBox 60 `shouldEqual` 8
      it "toBox 80" $ toBox 80 `shouldEqual` 8

-------------------------------------------------------------------
-- Get Indices
-------------------------------------------------------------------

    describe "Get Indices" do
      it "indicesRow 0" $ indicesRow 0 `shouldEqual` [0,1,2,3,4,5,6,7,8]
      it "indicesRow 8" $ indicesRow 8 `shouldEqual` [72,73,74,75,76,77,78,79,80]
      it "indicesCol 0" $ indicesCol 0 `shouldEqual` [0,9,18,27,36,45,54,63,72]
      it "indicesCol 8" $ indicesCol 8 `shouldEqual` [8,17,26,35,44,53,62,71,80]
      it "indicesBox 0" $ indicesBox 0 `shouldEqual` [0,1,2,9,10,11,18,19,20]
      it "indicesBox 8" $ indicesBox 8 `shouldEqual` [60,61,62,69,70,71,78,79,80]
      it "indicesPeers 56" $ indicesPeers 56 `shouldEqual` 
        [54,55,56,57,58,59,60,61,62,2,11,20,29,38,47,65,74,63,64,72,73]

-------------------------------------------------------------------
-- Update Board State
-------------------------------------------------------------------

    describe "Update Board State" do
      it "batchDropOptions" $ batchDropOptions 
        [Tuple 0 (Cell 511), Tuple 2 (Cell 503), Tuple 8 (Cell 503)] 
          startingBoard `shouldEqual` coerce
          [ 0,511,8,32,64,2,511,511,8
          , 16,511,511,128,511,511,511,256,32
          , 511,32,4,511,8,511,511,511,128
          , 4,128,2,1,511,511,256,32,511
          , 8,64,16,511,511,511,1,511,511
          , 256,1,511,2,511,8,16,511,511
          , 511,511,511,511,511,511,511,2,256
          , 511,511,1,511,511,511,64,8,4
          , 2,511,511,511,32,4,128,511,1
          ]