module App.Parseboards where

import Prelude

import Asset.SudokuBoard.Easy as Easy
import Asset.SudokuBoard.Hard as Hard
import Asset.SudokuBoard.Hardest as Hardest
import Data.Array (mapMaybe)
import Data.Either (hush)
import Sudoku.Puzzle (Puzzle, fromString)

hushErrorsParser :: Array String -> Array Puzzle
hushErrorsParser = mapMaybe (fromString >>> hush)

easyPuzzles :: Array Puzzle
easyPuzzles = hushErrorsParser Easy.asset

hardPuzzles :: Array Puzzle
hardPuzzles = hushErrorsParser Hard.asset

hardestPuzzles :: Array Puzzle
hardestPuzzles = hushErrorsParser Hardest.asset