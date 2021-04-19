-- | A puzzle bundles a board with a metaboard. 
-- | A metaboard can be used to implement memory for a board.
-- |
-- | For example: An algorithm may enter data into the metaboard of a puzzle so that
-- | the next time it runs, it will not check for patterns it has already found and
-- | implemented for that board.
module Sudoku.Puzzle where

import Prelude

import Data.Either (Either)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Error (Error)
import Sudoku.Board (Board)
import Sudoku.Board as Brd
import Sudoku.Cell (Cell)
import Sudoku.Group (Group)
import Sudoku.Index (Index)

type MetaBoard = 
  { tupleState :: Map Group (Map Int (Tuple Cell (Array Index)))
  , tupleCount :: Map Int Int
  , bruteForce ::
    { guessed :: Int
    , backtrack :: Int
    }
  }

type Puzzle = Tuple MetaBoard Board

blankMetaBoard :: MetaBoard
blankMetaBoard = 
  { tupleState: fromFoldable []
  , tupleCount: fromFoldable []
  , bruteForce: 
    { guessed: 0
    , backtrack: 0
    }
  }

fromBoard :: Board -> Puzzle
fromBoard board = Tuple blankMetaBoard board

fromString :: String -> Either Error Puzzle
fromString = Brd.fromString >>> map (Tuple blankMetaBoard)

