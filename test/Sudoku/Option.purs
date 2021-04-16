module Test.Sudoku.Option where

import Prelude

import Data.Maybe (Maybe(..))
import Sudoku.Option (allOptions, asString, boundedOption, indexOf, invalidOption, nthOption)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Sudoku Option" do
    describe "Smart Constructors" do
      it "just nthOption" $ nthOption 4 `shouldEqual` ( Just $ boundedOption 4 )
      it "nothing nthOption" $ nthOption (-1) `shouldEqual` Nothing
      it "nothing nthOption" $ nthOption 9 `shouldEqual` Nothing
      it "allOptions" $ allOptions `shouldEqual` (boundedOption <$>
        [0,1,2,3,4,5,6,7,8])
    describe "Utility" do
      it "as string" $ asString invalidOption `shouldEqual` "Ԑ"
      it "indexes" $ indexOf (boundedOption 4) `shouldEqual` 4