module Sudoku.Index 
  ( module IndexType

  , boundedIndex
  , boardIndices

  , toInt
  , toNumber
  ) where

import Prelude

import Data.Array (length, (..))
import Data.Int as Ints
import Safe.Coerce (coerce)
import Sudoku.Index.Internal (Index(..))
import Sudoku.Option (numOfOptions)
import Sudoku.Index.Internal (Index) as IndexType

-------------------------------------------------------------------
-- Smart Constructors for Arrays of Indices
-------------------------------------------------------------------

-- | Turn an int into an Index. Ints that are not valid Indexs return
-- | the nearest possible Index
boundedIndex :: Int -> Index
boundedIndex n
  | n >= length boardIndices = Index $ length boardIndices - 1
  | n < 0 = Index 0
  | otherwise = Index n

-- | An Array containing every valid index for a board
boardIndices :: Array Index
boardIndices = coerce $ 0 .. (numOfOptions * numOfOptions - 1)

-------------------------------------------------------------------
-- Convert from Index
-------------------------------------------------------------------

-- | Convert index to an Int
toInt :: Index -> Int
toInt = coerce

-- | Convert index to a Number
toNumber :: Index -> Number
toNumber = coerce Ints.toNumber