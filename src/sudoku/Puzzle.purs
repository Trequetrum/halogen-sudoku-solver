module Sudoku.Puzzle where

import Prelude

import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Error (Error)
import Sudoku.Board as Brd
import Sudoku.Board (Board)

type MetaBoard = 
  { metaData :: {}
  , metaBoard :: (Array {})
  }

type Puzzle = Tuple MetaBoard Board

fromString :: String -> Either Error Puzzle
fromString = Brd.fromString >>> map (Tuple { metaData: {}, metaBoard: [] })