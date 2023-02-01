module Sudoku.Index.Internal where

import Prelude

import Data.Array ((..))
import Data.Int as Ints
import Data.Number (sqrt)
import Safe.Coerce (coerce)
import Sudoku.Option (numOfOptions)

newtype Index = Index Int

derive newtype instance eqIndex :: Eq Index
derive newtype instance ordIndex :: Ord Index
derive newtype instance showIndex :: Show Index

-- | Helper function that ensures a valid group is indicated by an Int
-- | If not, it uses the nearest valid group
boundedGroup :: (Int -> Array Index) -> Int -> Array Index
boundedGroup fn int = 
  if int < 0 
  then fn 0
  else if int >= numOfOptions 
  then fn $ numOfOptions -1
  else fn int

-- | An Array containing the indices for the given row index
indicesRow :: Int -> Array Index
indicesRow = boundedGroup indices
  where
    indices n = coerce $ (n * numOfOptions) .. (n * numOfOptions + numOfOptions - 1)

-- | An Array containing the indices for the given column index
indicesCol :: Int -> Array Index
indicesCol = boundedGroup indices
  where
    indices n = coerce $ (_ * numOfOptions) >>> (_ + n) <$> 0 .. (numOfOptions - 1)

-- | An Array containing the indices for the given box index
indicesBox :: Int -> Array Index
indicesBox = boundedGroup indices
  where 
    indices n = coerce $ boxIndex <$> boxRange <*> boxRange
      where
        boxRange :: Array Int
        boxRange = 0 .. (root - 1)

        boxOffset :: Int
        boxOffset = n / root * root * numOfOptions +
          (n `mod` root) * root

        boxIndex :: Int -> Int -> Int
        boxIndex r c = boxOffset + numOfOptions * r + c

        root :: Int
        root = Ints.floor $ sqrt $ Ints.toNumber numOfOptions