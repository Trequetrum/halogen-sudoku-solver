module Test.Sudoku.Cell where

import Prelude

import Sudoku.Cell (asOptions, countOptions, dropOptions, hasOption, isSuperset, notDisjoint, toCell, trustFirstOption)
import Sudoku.Cell.Internal (Cell(..))
import Sudoku.Option (boundedOption)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Sudoku Cell" do
    describe "Smart Constructors" do pure unit
    describe "Operations" do
      it "toCell 0" $ toCell (boundedOption 0) `shouldEqual` Cell 1
      it "toCell 3" $ toCell (boundedOption 2) `shouldEqual` Cell 4
      it "toCell 9" $ toCell (boundedOption 8) `shouldEqual` Cell 256
      it "dropOptions 56 56" $ dropOptions (Cell 56) (Cell 56) `shouldEqual` Cell 0
      it "dropOptions 56 511" $ dropOptions (Cell 56) (Cell 511) `shouldEqual` Cell 455
      it "dropOptions 312 496" $ dropOptions (Cell 312) (Cell 496) `shouldEqual` Cell 192
      it "dropOptions 313 340" $ dropOptions (Cell 313) (Cell 340) `shouldEqual` Cell 68
      it "asOptions 147" $ show (asOptions (Cell 147)) `shouldEqual` "[1,2,5,8]"
      it "asOptions 286" $ show (asOptions (Cell 286)) `shouldEqual` "[2,3,4,5,9]"
      it "trustFirstOption 147" $ trustFirstOption (Cell 144) `shouldEqual` boundedOption 4
      it "trustFirstOption 286" $ trustFirstOption (Cell 286) `shouldEqual` boundedOption 1
      it "countOptions' 147" $ countOptions (Cell 147) `shouldEqual` 4
      it "countOptions' 286" $ countOptions (Cell 286) `shouldEqual` 5
      it "countOptions 147" $ countOptions (Cell 147) `shouldEqual` 4
      it "countOptions 286" $ countOptions (Cell 286) `shouldEqual` 5
    describe "Predicates" do 
      it "hasOption 1 1" $ hasOption (boundedOption 0) (Cell 1) `shouldEqual` true
      it "hasOption 1 511" $ hasOption (boundedOption 0) (Cell 511) `shouldEqual` true
      it "hasOption 5 511" $ hasOption (boundedOption 4) (Cell 511) `shouldEqual` true
      it "hasOption 5 1" $ hasOption (boundedOption 4) (Cell 1) `shouldEqual` false
      it "isSuperset 147 187" $ isSuperset (Cell 187) (Cell 147) `shouldEqual` true
      it "isSuperset 187 147" $ isSuperset (Cell 147) (Cell 187) `shouldEqual` false
      it "notDisjoint 21 16" $ notDisjoint (Cell 21) (Cell 16) `shouldEqual` true
      it "notDisjoint 21 10" $ notDisjoint (Cell 21) (Cell 10) `shouldEqual` false
    describe "Internal" do pure unit