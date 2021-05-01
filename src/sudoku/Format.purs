module Sudoku.Format where

import Prelude

import Data.Array (replicate, splitAt)
import Data.Int (binary, fromStringAs, toNumber, toStringAs)
import Data.Maybe (fromMaybe)
import Data.String (codePointFromChar, fromCodePointArray, joinWith)
import Data.String as String
import Data.Tuple (snd)
import Math (floor, sqrt, (%))
import Safe.Coerce (coerce)
import Stateful (Stateful, constructorString, unwrapStateful)
import Sudoku.Board (Board, mapBoard)
import Sudoku.OSet (asOptions, hasOption)
import Sudoku.OSet.Internal (OSet(..))
import Sudoku.Group (groupId, toColumn, toRow)
import Sudoku.Index (Index)
import Sudoku.Option (allOptions, asString, indexOf, numOfOptions)
import Sudoku.Option as Optn
import Sudoku.Puzzle (Puzzle)
import Utility (inc)

-------------------------------------------------------------------
-- Display Helpers
-------------------------------------------------------------------

rightSudokuBoxBorder :: Index -> Boolean
rightSudokuBoxBorder = beforeSudokuBorder (toColumn >>> groupId >>> indexOf)

bottomSudokuBoxBorder :: Index -> Boolean
bottomSudokuBoxBorder = beforeSudokuBorder (toRow >>> groupId >>> indexOf)

beforeSudokuBorder :: (Index -> Int) -> Index -> Boolean
beforeSudokuBorder axis index =
  pos % root == 0.0 && 
  pos / root < root
  where
    pos = inc $ toNumber $ axis index
    root = floor $ sqrt $ toNumber numOfOptions 

-------------------------------------------------------------------
-- Convert to/from string
-------------------------------------------------------------------

setAsBinary :: OSet -> String
setAsBinary n = prefill (numOfOptions - String.length asBinary) <> asBinary
  where
    asBinary :: String
    asBinary = toStringAs binary $ coerce n
    prefill :: Int -> String
    prefill num 
      | num > 0 = fromCodePointArray $ replicate num $ codePointFromChar '0'
      | otherwise = ""

binaryAsCell :: String -> OSet
binaryAsCell = fromStringAs binary >>> fromMaybe (-1) >>> OSet

setAsOptions :: OSet -> String
setAsOptions = asOptions >>> map Optn.asString >>> joinWith ""

statefulPuzzleToOptionsString :: Stateful Puzzle -> String
statefulPuzzleToOptionsString p = constructorString p <> ": \n" <> (puzzleToOptionsString $ unwrapStateful p)

puzzleToOptionsString :: Puzzle -> String
puzzleToOptionsString = snd >>> boardToOptionsString

boardToOptionsString :: Board -> String
boardToOptionsString = mapBoard setAsOptions >>> joinWith "\" \""

-------------------------------------------------------------------
-- Console output for Sudoku Puzzles
-------------------------------------------------------------------

statefulPuzzleToString :: Stateful Puzzle -> String
statefulPuzzleToString p = constructorString p <> ": \n" <> (puzzleToString $ unwrapStateful p)

puzzleToString :: Puzzle -> String
puzzleToString = snd >>> boardToString

setToString :: OSet -> String
setToString set = let 
  mapOption opt = if hasOption opt set 
    then asString opt 
    else "."
  in joinWith "" $ mapOption <$> allOptions

boardToString :: Board -> String
boardToString board = display "" $ mapBoard setToString board
  where
    display :: String -> (Array String) -> String
    display acc [] = acc
    display acc arr = let
      split = splitAt numOfOptions arr
      in display 
        (acc <> (joinWith " " split.before) <> "\n")
        (split.after)




