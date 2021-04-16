module Test.Sudoku.Index where

import Prelude

import Sudoku.Index (boundedIndex)
import Sudoku.Index.Internal (indicesBox, indicesCol, indicesRow)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Sudoku Index" do
    describe "Smart Constructors" do 
      it "Bound index 1" $ boundedIndex (-5) `shouldEqual` boundedIndex 0
      it "Bound index 2" $ boundedIndex 500 `shouldEqual` (boundedIndex 80)
      it "Bound index 3" $ boundedIndex 21 `shouldEqual` (boundedIndex 21)
    describe "Internal" do 
      it "indicesRow 0" $ indicesRow 0 `shouldEqual` (boundedIndex <$> [0,1,2,3,4,5,6,7,8])
      it "indicesRow 8" $ indicesRow 8 `shouldEqual` (boundedIndex <$> [72,73,74,75,76,77,78,79,80])
      it "indicesCol 0" $ indicesCol 0 `shouldEqual` (boundedIndex <$> [0,9,18,27,36,45,54,63,72])
      it "indicesCol 8" $ indicesCol 8 `shouldEqual` (boundedIndex <$> [8,17,26,35,44,53,62,71,80])
      it "indicesBox 0" $ indicesBox 0 `shouldEqual` (boundedIndex <$> [0,1,2,9,10,11,18,19,20])
      it "indicesBox 8" $ indicesBox 8 `shouldEqual` (boundedIndex <$> [60,61,62,69,70,71,78,79,80])