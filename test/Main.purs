module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Sudoku.Board as SudokuBoard
import Test.Sudoku.OSet as SudokuOSet
import Test.Sudoku.Format as SudokuFormat
import Test.Sudoku.Group as SudokuGroup
import Test.Sudoku.Index as SudokuIndex
import Test.Sudoku.Option as SudokuOption
import Test.Sudoku.Puzzle as SudokuPuzzle

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  SudokuBoard.spec
  SudokuOSet.spec
  SudokuGroup.spec
  SudokuIndex.spec
  SudokuOption.spec
  SudokuPuzzle.spec
  SudokuFormat.spec

