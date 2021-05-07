module Sudoku.Format where

import Prelude

import Data.Array (replicate, splitAt)
import Data.Int (binary, fromStringAs, toNumber, toStringAs)
import Data.Maybe (fromMaybe)
import Data.String (codePointFromChar, fromCodePointArray, joinWith)
import Data.String as Strings
import Data.Tuple (Tuple(..), snd)
import Math (floor, sqrt, (%))
import Safe.Coerce (coerce)
import Stateful (Stateful, constructorString, unwrapStateful)
import Sudoku.Board (Board, indexedCells, mapBoard)
import Sudoku.Group (groupId, toColumn, toRow)
import Sudoku.Index (Index, toInt)
import Sudoku.OSet (asOptions, hasOption, countOptions, firstOption)
import Sudoku.OSet.Internal (OSet(..))
import Sudoku.Option (allOptions, asString, indexOf, invalidOption, numOfOptions)
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
setAsBinary n = prefill (numOfOptions - Strings.length asBinary) <> asBinary
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

setToOptionString :: OSet -> String
setToOptionString set = case countOptions set of
  1 -> asString $ fromMaybe invalidOption $ firstOption set
  _ -> "."

boardToForcedString :: Board -> String
boardToForcedString board = joinWith "" $ stringify <$> indexedCells board
  where
    stringify (Tuple i set) = setToOptionString set <> 
      ( if (toInt i + 1) `mod` 9 == 0 
        then " "
        else ""
      ) <>
      ( if (toInt i + 1) `mod` 27 == 0 
        then "\n"
        else ""
      )
  
boardToIntString :: Board -> String
boardToIntString board = "[ " <> (joinWith ", " $ stringify <$> indexedCells board) <> "]"
  where
    stringify (Tuple i (OSet int)) = (show int) <>
      ( if Strings.length (show int) == 1
        then "  "
        else if Strings.length (show int) == 2
        then " "
        else "") <>
      ( if (toInt i + 1) `mod` 9 == 0 
        then "\n"
        else ""
      )



