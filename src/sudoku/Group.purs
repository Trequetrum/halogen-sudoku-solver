module Sudoku.Group 
  ( Group

  , row
  , column
  , box

  , rows
  , columns
  , boxes
  , groups

  , toRow
  , toColumn
  , toBox
  , toGroups

  , toGroupsIntersection
  , groupIndices
  , groupId
  , peerIndices
  , exPeerIndices

  , asIdString
  ) where

import Prelude

import Data.Array (filter, foldl, head, intersect, nub, tail)
import Data.Int as Ints
import Data.Maybe (Maybe(..), fromMaybe)
import Math (sqrt)
import Sudoku.Index (toInt)
import Sudoku.Index.Internal (Index, indicesBox, indicesCol, indicesRow)
import Sudoku.Option (Option, allOptions, asString, boundedOption, indexOf, numOfOptions)

data Group
  = Row Option (Array Index)
  | Column Option (Array Index)
  | Box Option (Array Index)

instance showGroup :: Show Group where
  show :: Group -> String
  show (Row o a) = "(Row " <> show o <> " " <> show a <> ")"
  show (Column o a) = "(Column " <> show o <> " " <> show a <> ")"
  show (Box o a) = "(Box " <> show o <> " " <> show a <> ")"

instance eqGroup :: Eq Group where
  eq :: Group -> Group -> Boolean
  eq (Row _ _) (Column _ _) = false
  eq (Row _ _) (Box _ _) = false

  eq (Column _ _) (Row _ _) = false
  eq (Column _ _) (Box _ _) = false

  eq (Box _ _) (Row _ _) = false
  eq (Box _ _) (Column _ _) = false

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
-- There are exactly as many groups of each type as their are symbols
-- in the set of Options. Instead of giving groups a ID of their own
-- (As I maybe should). I re-use the options as labels for each type
-- of group. 
-------------------------------------------------------------------

-- | Get the row corresponding to it's Option Label
row :: Option -> Group
row n = Row n $ indicesRow $ indexOf n

-- | Get the column corresponding to it's Option Label
column :: Option -> Group
column n = Column n $ indicesCol $ indexOf n

-- | Get the box corresponding to it's Option Label
box :: Option -> Group
box n = Box n $ indicesBox $ indexOf n

-- | An array of every row
rows :: Array Group
rows = row <$> allOptions

-- | An array of every column
columns :: Array Group
columns = column <$> allOptions

-- | An array of every box 
boxes :: Array Group
boxes = box <$> allOptions

-- | An array of every group
groups :: Array Group
groups = rows <> columns <> boxes

-------------------------------------------------------------------
-- Index mappings
-- Index -> Row Column & Box
-------------------------------------------------------------------

-- | Returns the row this index is a part of
toRow :: Index -> Group
toRow i = row $ boundedOption $ indexed
  where
    indexed = (toInt i) / numOfOptions

-- | Returns the column this index is a part of
toColumn :: Index -> Group
toColumn i = column $ boundedOption $ indexed
  where
    indexed = (toInt i) `mod` numOfOptions

-- | Returns the box this index is a part of
toBox :: Index -> Group
toBox index = box $ boundedOption $ indexed
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

toGroupsIntersection :: Array Index -> Array Group
toGroupsIntersection indices = folder (head grouped) (fromMaybe [] $ tail grouped)
  where
    grouped :: Array (Array Group)
    grouped = toGroups <$> indices

    folder :: Maybe (Array Group) -> Array (Array Group) -> Array Group
    folder Nothing  _ = []
    folder (Just h) t = foldl intersect h t

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
peerIndices :: Index -> Array Index
peerIndices i = nub $ toGroups i >>= groupIndices

-- | An Array containing the indices for the peers of a given index
-- | excluding itself in the list of peers
exPeerIndices :: Index -> Array Index
exPeerIndices i = filter (notEq i) $ peerIndices i

-------------------------------------------------------------------
-- Formatting for Groups
-------------------------------------------------------------------

asIdString :: Group -> String
asIdString (Row o _) = "Row " <> asString o 
asIdString (Column o _) = "Column " <> asString o 
asIdString (Box o _) = "Box " <> asString o 