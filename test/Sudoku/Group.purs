module Test.Sudoku.Group where

import Prelude

import Sudoku.Group (box, column, exPeerIndices, peerIndices, row, toBox, toColumn, toRow)
import Sudoku.Index (boundedIndex)
import Sudoku.Index.Internal (Index(..))
import Sudoku.Option (boundedOption)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Sudoku Group" do
    describe "Smart Constructors" do
      it "Row" $ show (row $ boundedOption 5) `shouldEqual` "(Row 6 [45,46,47,48,49,50,51,52,53])"
      it "Column" $ show (column $ boundedOption 5) `shouldEqual` "(Column 6 [5,14,23,32,41,50,59,68,77])"
      it "Box" $ show (box $ boundedOption 5) `shouldEqual` "(Box 6 [33,34,35,42,43,44,51,52,53])"
    describe "Index mappings" do
      it "toRow" $ toRow (Index 58) `shouldEqual` row (boundedOption 6)
      it "toColumn" $ toColumn (Index 58) `shouldEqual` column (boundedOption 4)
      it "toBox" $ toBox (Index 58) `shouldEqual` box (boundedOption 7)
    describe "Group Operations" do
      it "indicesPeers 56" $ peerIndices (boundedIndex 56) `shouldEqual` (boundedIndex <$> 
        [54,55,56,57,58,59,60,61,62,2,11,20,29,38,47,65,74,63,64,72,73])
      it "exPeerIndices 56" $ exPeerIndices (boundedIndex 56) `shouldEqual` (boundedIndex <$> 
        [54,55,57,58,59,60,61,62,2,11,20,29,38,47,65,74,63,64,72,73])