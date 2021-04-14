module Sudoku.Group where

import Prelude

import Data.Array (filter, nub)
import Data.Int as Ints
import Math (sqrt)
import Sudoku.Option (Option, allOptions, boundedOption, indexOf, numOfOptions)
import Sudoku.Index (toInt)
import Sudoku.Index.Internal (Index, indicesCol, indicesRow)

data Group
  = Row Option (Array Index)
  | Column Option (Array Index)
  | Box Option (Array Index)

instance eqGroup :: Eq Group where
  eq :: Group -> Group -> Boolean
  eq left right = groupId left == groupId right

instance ordGroup :: Ord Group where
  compare :: Group -> Group -> Ordering
  compare (Row _ _) (Column _ _) = GT
  compare (Row _ _) (Box _ _) = GT
  compare (Row left _) (Row right _) = compare left right

  compare (Column _ _) (Row _ _) = LT
  compare (Column _ _) (Box _ _) = GT
  compare (Column left _) (Column right _) = compare left right

  compare (Box _ _) (Row _ _) = LT
  compare (Box _ _) (Column _ _) = LT
  compare (Box left _) (Box right _) = compare left right

-------------------------------------------------------------------
-- Group Smart Constructors
-------------------------------------------------------------------

row :: Option -> Group
row n = Row n $ indicesRow $ indexOf n

column :: Option -> Group
column n = Column n $ indicesCol $ indexOf n

box :: Option -> Group
box n = Box n $ indicesCol $ indexOf n

-- | An array of every row of indices
rows :: Array Group
rows = row <$> allOptions

-- | An array of every column of indices
columns :: Array Group
columns = column <$> allOptions

-- | An array of every box of indices
boxes :: Array Group
boxes = box <$> allOptions

-- | An array of every group of indices, note that every index will
-- | appear once in each type of group
groups :: Array Group
groups = rows <> columns <> boxes

-------------------------------------------------------------------
-- Index mappings
-- Index -> Row Column & Box
-------------------------------------------------------------------

-- | Returns the row this index is a part of
toRow :: Index -> Group
toRow i = row $ boundedOption $ indexed + 1
  where
    indexed = (toInt i) / numOfOptions

-- | Returns the column this index is a part of
toColumn :: Index -> Group
toColumn i = column $ boundedOption $ indexed + 1 
  where
    indexed = (toInt i) `mod` numOfOptions

-- | Returns the box this index is a part of
toBox :: Index -> Group
toBox index = box $ boundedOption $ indexed + 1
  where
    i = toInt index
    root = Ints.floor $ sqrt $ Ints.toNumber numOfOptions
    indexed = root *
      ((i / numOfOptions) / root) +
      ((i / root) `mod` root)

-- | Return all the groups this index is a part of
toGroups :: Index -> Array Group
toGroups index = 
  [ toRow index
  , toColumn index
  , toBox index
  ]

-------------------------------------------------------------------
-- Group Operations
-------------------------------------------------------------------

groupIndices :: Group -> Array Index
groupIndices (Row _ a) = a 
groupIndices (Column _ a) = a
groupIndices (Box _ a) = a

groupId :: Group -> Option
groupId (Row o _) = o 
groupId (Column o _) = o
groupId (Box o _) = o

-- | An Array containing the indices for the peers of a given index
-- | including itself in the list of peers
indicesPeers :: Index -> Array Index
indicesPeers i = nub $ toGroups i >>= groupIndices

-- | An Array containing the indices for the peers of a given index
-- | excluding itself in the list of peers
indicesExPeers :: Index -> Array Index
indicesExPeers i = filter (notEq i) $ indicesPeers i