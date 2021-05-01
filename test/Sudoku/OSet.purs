module Test.Sudoku.OSet where

import Prelude

import Sudoku.OSet (asOptions, countOptions, dropOptions, hasOption, isSuperset, notDisjoint, toOSet, trustFirstOption)
import Sudoku.OSet.Internal (OSet(..))
import Sudoku.Option (boundedOption)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Sudoku OSet" do
    describe "Smart Constructors" do pure unit
    describe "Operations" do
      it "toOSet 0" $ toOSet (boundedOption 0) `shouldEqual` OSet 1
      it "toOSet 3" $ toOSet (boundedOption 2) `shouldEqual` OSet 4
      it "toOSet 9" $ toOSet (boundedOption 8) `shouldEqual` OSet 256
      it "dropOptions 56 56" $ dropOptions (OSet 56) (OSet 56) `shouldEqual` OSet 0
      it "dropOptions 56 511" $ dropOptions (OSet 56) (OSet 511) `shouldEqual` OSet 455
      it "dropOptions 312 496" $ dropOptions (OSet 312) (OSet 496) `shouldEqual` OSet 192
      it "dropOptions 313 340" $ dropOptions (OSet 313) (OSet 340) `shouldEqual` OSet 68
      it "asOptions 147" $ show (asOptions (OSet 147)) `shouldEqual` "[1,2,5,8]"
      it "asOptions 286" $ show (asOptions (OSet 286)) `shouldEqual` "[2,3,4,5,9]"
      it "trustFirstOption 147" $ trustFirstOption (OSet 144) `shouldEqual` boundedOption 4
      it "trustFirstOption 286" $ trustFirstOption (OSet 286) `shouldEqual` boundedOption 1
      it "countOptions' 147" $ countOptions (OSet 147) `shouldEqual` 4
      it "countOptions' 286" $ countOptions (OSet 286) `shouldEqual` 5
      it "countOptions 147" $ countOptions (OSet 147) `shouldEqual` 4
      it "countOptions 286" $ countOptions (OSet 286) `shouldEqual` 5
    describe "Predicates" do 
      it "hasOption 1 1" $ hasOption (boundedOption 0) (OSet 1) `shouldEqual` true
      it "hasOption 1 511" $ hasOption (boundedOption 0) (OSet 511) `shouldEqual` true
      it "hasOption 5 511" $ hasOption (boundedOption 4) (OSet 511) `shouldEqual` true
      it "hasOption 5 1" $ hasOption (boundedOption 4) (OSet 1) `shouldEqual` false
      it "isSuperset 147 187" $ isSuperset (OSet 187) (OSet 147) `shouldEqual` true
      it "isSuperset 187 147" $ isSuperset (OSet 147) (OSet 187) `shouldEqual` false
      it "notDisjoint 21 16" $ notDisjoint (OSet 21) (OSet 16) `shouldEqual` true
      it "notDisjoint 21 10" $ notDisjoint (OSet 21) (OSet 10) `shouldEqual` false
    describe "Internal" do pure unit