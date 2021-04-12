module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Sudoku.Board as SudokuBoard
import Test.Sudoku.Format as SudokuFormat

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  SudokuBoard.spec
  SudokuFormat.spec

