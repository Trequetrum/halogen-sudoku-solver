-- | A puzzle bundles a board with a metaboard. 
-- | A metaboard can be used to implement memory for a board.
-- |
-- | For example: An algorithm may enter data into the metaboard of a puzzle so that
-- | the next time it runs, it will not check for patterns it has already found and
-- | implemented for that board.
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