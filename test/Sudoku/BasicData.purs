module Test.Basic.Data where

import Prelude

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Safe.Coerce (coerce)
import Sudoku.Board (Board, fromCells, modifyAtIndex, unconstrainedBoard)
import Sudoku.OSet (dropOptions, setOptions)
import Sudoku.OSet.Internal (OSet(..))
import Sudoku.Index (boundedIndex)
import Sudoku.Puzzle (Puzzle, fromBoard)
import Sudoku.Puzzle as Puzz

startingBoardString :: String
startingBoardString = """
  004 | 672 | 000
  500 | 800 | 096
  063 | 040 | 008
  ----+-----+----
  382 | 100 | 960
  475 | 000 | 100
  910 | 204 | 500
  ----+-----+----
  000 | 000 | 029
  001 | 000 | 743
  200 | 063 | 801
"""

notValidBoardString :: String
notValidBoardString = """
  004 | 672 | 000
  500 | 800 | 096
  063 | 040 | 008
  ----+-----+----
  382 | 106 | 960
  475 | 000 | 100
  910 | 204 | 500
  ----+-----+----
  000 | 000 | 029
  001 | 000 | 743
  200 | 063 | 801
"""

startingBoardSolvedString :: String
startingBoardSolvedString = """
  894 | 672 | 315
  527 | 831 | 496
  163 | 549 | 278
  ----+-----+----
  382 | 157 | 964
  475 | 396 | 182
  916 | 284 | 537
  ----+-----+----
  738 | 415 | 629
  651 | 928 | 743
  249 | 763 | 851
"""

startingBoardAlmostSolvedString :: String
startingBoardAlmostSolvedString = """
  894 | 6.2 | 315
  527 | 831 | 496
  163 | 549 | 278
  ----+-----+----
  382 | 157 | 964
  475 | 396 | 182
  916 | 284 | 537
  ----+-----+----
  738 | 415 | 629
  651 | 928 | 743
  249 | 763 | 851
"""

hardBoardString :: String
hardBoardString = """
  4.....8.5 .3....... ...7..... 
  .2.....6. ....8.4.. ....1.... 
  ...6.3.7. 5..2..... 1.4......
"""

hardBoardSolvedString :: String
hardBoardSolvedString = """
 417369825 632158947 958724316 
 825437169 791586432 346912758 
 289643571 573291684 164875293
"""

{- Indices laid out for visual reference
0, 1, 2,   3, 4, 5,   6, 7, 8,
9, 10,11,  12,13,14,  15,16,17,
18,19,20,  21,22,23,  24,25,26,

27,28,29,  30,31,32,  33,34,35,
36,37,38,  39,40,41,  42,43,44,
45,46,47,  48,49,50,  51,52,53,

54,55,56,  57,58,59,  60,61,62,
63,64,65,  66,67,68,  69,70,71,
72,73,74,  75,76,77,  78,79,80
-}

startingBoard :: Board
startingBoard = fromMaybe unconstrainedBoard $ fromCells $ coerce
  [ 511,511,8,32,64,2,511,511,511
  , 16,511,511,128,511,511,511,256,32
  , 511,32,4,511,8,511,511,511,128
  , 4,128,2,1,511,511,256,32,511
  , 8,64,16,511,511,511,1,511,511
  , 256,1,511,2,511,8,16,511,511
  , 511,511,511,511,511,511,511,2,256
  , 511,511,1,511,511,511,64,8,4
  , 2,511,511,511,32,4,128,511,1
  ]

startingBoard' :: Board
startingBoard' = fromMaybe unconstrainedBoard $ fromCells $ coerce
  [ 509,509,8,32,64,2,509,509,509
  , 16,511,511,128,511,511,511,256,32
  , 511,32,4,511,8,511,511,511,128
  , 4,128,2,1,511,511,256,32,511
  , 8,64,16,511,511,511,1,511,511
  , 256,1,511,2,511,8,16,511,511
  , 511,511,511,511,511,511,511,2,256
  , 511,511,1,511,511,511,64,8,4
  , 2,511,511,511,32,4,128,511,1
  ]

dummyPuzzle :: Puzzle
dummyPuzzle = fromBoard $ 
  fromMaybe unconstrainedBoard $ fromCells $ coerce
  [ 1,1,1,1,1,1,1,1,1
  , 1,1,1,1,1,1,1,1,1
  , 1,1,1,1,1,1,1,1,1
  , 1,1,1,1,1,1,1,1,1
  , 1,1,1,1,1,1,1,1,1
  , 1,1,1,1,1,1,1,1,1
  , 1,1,1,1,1,1,1,1,1
  , 1,1,1,1,1,1,1,1,1
  , 1,1,1,1,1,1,1,1,1
  ]

startingPuzzle :: Puzzle
startingPuzzle = fromBoard startingBoard

startingPuzzle' :: Puzzle
startingPuzzle' = fromBoard startingBoard'

hardBoardPuzzle :: Puzzle
hardBoardPuzzle = fromRight dummyPuzzle $ Puzz.fromString hardBoardString

startingPuzzleSolved :: Puzzle
startingPuzzleSolved = fromBoard startingBoardSolved

startingBoardSolved :: Board
startingBoardSolved = fromMaybe unconstrainedBoard $ fromCells $ coerce
  [ 128,256,8,32,64,2,4,1,16
  , 16,2,64,128,4,1,8,256,32
  , 1,32,4,16,8,256,2,64,128
  , 4,128,2,1,16,64,256,32,8
  , 8,64,16,4,256,32,1,128,2
  , 256,1,32,2,128,8,16,4,64
  , 64,4,128,8,1,16,32,2,256
  , 32,16,1,256,2,128,64,8,4
  , 2,8,256,64,32,4,128,16,1
  ]

badCellboard1 :: Board
badCellboard1 = modifyAtIndex (dropOptions $ OSet 511) (boundedIndex 0) startingBoard

badCellboard2 :: Board
badCellboard2 = modifyAtIndex (setOptions $ OSet 512) (boundedIndex 0) startingBoard

forcedCellDuplicateBoard :: Board
forcedCellDuplicateBoard = fromMaybe unconstrainedBoard $ fromCells $ coerce
  [ 256,511,8,  32, 64, 2,  511,511,511
  , 16, 511,511,128,511,511,511,256,32
  , 511,32, 4,  511,8,  511,511,511,128
  , 4,  128,2,  1,  511,511,256,32, 511
  , 8,  64, 16, 511,511,511,1,  511,511
  , 256,1,  511,2,  511,8,  16, 511,511
  , 511,511,511,511,511,511,511,2,  256
  , 511,511,1,  511,511,511,64, 8,  4
  , 2,  511,511,511,32, 4,  128,511,1
  ]

forcedCellDuplicateBoard2 :: Board
forcedCellDuplicateBoard2 = fromMaybe unconstrainedBoard $ fromCells $ coerce
  [ 511,511,8,32,64,2,511,511,511
  , 16,511,511,128,511,511,511,256,32
  , 511,32,4,511,8,511,511,511,128
  , 4,128,2,1,511,511,256,32,511
  , 8,64,16,511,511,511,1,16,511
  , 256,1,511,2,511,8,16,511,511
  , 511,511,511,511,511,511,511,2,256
  , 511,511,1,511,511,511,64,8,4
  , 2,511,511,511,32,4,128,511,1
  ]

nTupleDuplicateBoard :: Board
nTupleDuplicateBoard = fromMaybe unconstrainedBoard $ fromCells $ coerce
  [ 511,511,8,32,64,2,511,511,511
  , 16,511,511,128,511,511,511,256,32
  , 511,32,4,511,8,511,511,511,128
  , 33,128,2,1,33,511,256,33,511
  , 8,64,16,511,511,511,1,511,511
  , 256,1,511,2,511,8,16,511,511
  , 511,511,511,511,511,511,511,2,256
  , 511,511,1,511,511,511,64,8,4
  , 2,511,511,511,32,4,128,511,1
  ]