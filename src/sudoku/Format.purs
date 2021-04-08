module Sudoku.Format where

import Prelude

import Data.Array (filter, find, length, replicate, splitAt)
import Data.Either (Either(..))
import Data.Int (binary, decimal, fromString, fromStringAs, toNumber, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), codePointFromChar, fromCodePointArray, joinWith, split)
import Data.String as String
import Data.Tuple (Tuple(..), snd)
import Error (Error(..))
import Safe.Coerce (coerce)
import Stateful (Stateful(..))
import Sudoku.Common (Board, Cell(..), Option(..), Puzzle, allOptions, allOptionsInt, asOptions, boardRoot, boardSize, hasOption, toCell, toCol, toRow)
import Utility (inc)
import Math((%))

-------------------------------------------------------------------
-- Display Helpers
-------------------------------------------------------------------

rightSudokuBoxBorder :: Int -> Boolean
rightSudokuBoxBorder = beforeSudokuBorder toCol

bottomSudokuBoxBorder :: Int -> Boolean
bottomSudokuBoxBorder = beforeSudokuBorder toRow

beforeSudokuBorder :: (Int -> Int) -> Int -> Boolean
beforeSudokuBorder axis index =
  pos % root == 0.0 && 
  pos / root < root
  where
    pos = inc $ toNumber $ axis index
    root = toNumber boardRoot 

-------------------------------------------------------------------
-- Convert to/from string
-------------------------------------------------------------------

cellAsBinary :: Cell -> String
cellAsBinary n = prefill (boardSize - String.length asBinary) <> asBinary
  where
    asBinary :: String
    asBinary = toStringAs binary $ coerce n
    prefill :: Int -> String
    prefill num 
      | num > 0 = fromCodePointArray $ replicate num $ codePointFromChar '0'
      | otherwise = ""

binaryAsCell :: String -> Cell
binaryAsCell = fromStringAs binary >>> fromMaybe (-1) >>> Cell

cellAsOptions :: Cell -> String
cellAsOptions = (coerce asOptions) >>> map (toStringAs decimal) >>> joinWith ""

optionString :: Option -> String
optionString = coerce $ toStringAs decimal

statefulPuzzleToOptionsString :: Stateful Puzzle -> String
statefulPuzzleToOptionsString (Advancing p) = "Advancing: \n" <> puzzleToOptionsString p
statefulPuzzleToOptionsString (Stable p) = "Stable: \n" <> puzzleToOptionsString p
statefulPuzzleToOptionsString (Finished p) = "Finished: \n" <> puzzleToOptionsString p

puzzleToOptionsString :: Puzzle -> String
puzzleToOptionsString = snd >>> boardToOptionsString

boardToOptionsString :: Board -> String
boardToOptionsString = map cellAsOptions >>> joinWith "\" \""

-------------------------------------------------------------------
-- Console output for Sudoku Puzzles
-------------------------------------------------------------------

statefulPuzzleToString :: Stateful Puzzle -> String
statefulPuzzleToString (Advancing p) = "Advancing: \n" <> puzzleToString p
statefulPuzzleToString (Stable p) = "Stable: \n" <> puzzleToString p
statefulPuzzleToString (Finished p) = "Finished: \n" <> puzzleToString p

puzzleToString :: Puzzle -> String
puzzleToString = snd >>> boardToString

boardToString :: Board -> String
boardToString board = display "" $ cellToString <$> board
  where
    display :: String -> (Array String) -> String
    display acc [] = acc
    display acc arr = let
      split = splitAt boardSize arr
      in display 
        (acc <> (joinWith " " split.before) <> "\n")
        (split.after)

cellToString :: Cell -> String
cellToString cell = let 
  mapOption opt = if hasOption opt cell 
    then toStringAs decimal $ coerce opt 
    else "."
  in joinWith "" $ mapOption <$> allOptions
  
-------------------------------------------------------------------
-- Parse
-------------------------------------------------------------------
-- For the textual format, we allow a string of characters with 1-9 
-- indicating a digit, and a 0 or period specifying an empty cell. 
-- All other characters are ignored. This includes spaces, newlines, 
-- dashes, and bars
-------------------------------------------------------------------

parseBoard :: String -> Either Error Board
parseBoard =
  split (Pattern "") >>>
  filter (\v -> (find (eq v) keepVals) /= Nothing) >>>
  map (fromString >>> fromMaybe 0) >>>
  (\v -> if fullLength == length v
    then Right v
    else Left $ Error "Parsing Sudoku Board" ("Input string contained " <> 
      show (length v) <> "/" <> show fullLength <> " valid characters")
  ) >>>
  map (map makeStarterCell)
  where
    keepVals = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."]
    fullLength = boardSize * boardSize
    makeStarterCell :: Int -> Cell
    makeStarterCell n
      | n > 0 && n <= boardSize = toCell $ Option n
      | otherwise = Cell allOptionsInt

parsePuzzle :: String -> Either Error Puzzle
parsePuzzle = parseBoard >>> (map $ Tuple { metaData: {}, metaBoard: [] })

