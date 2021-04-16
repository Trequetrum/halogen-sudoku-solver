module Test.Sudoku.Format where

import Prelude

import Data.Either (Either(..))
import Test.Spec (Spec, describe)

eitherIsLeft :: forall a b. Either a b -> Boolean
eitherIsLeft (Left _) = true
eitherIsLeft _ = false

spec :: Spec Unit
spec =
  describe "Sudoku Format" do pure unit
    