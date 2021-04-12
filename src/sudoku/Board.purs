-- | 
-- | A sudoku board is an collection of cells.
-- | 
-- | * Board: Collection of all cells, ordered by Index
-- | * Index: A unique id label for a cell. Can be converted to Row/Column/Box
-- | * Cell: Value of a board at an Index (label). This is an
-- |      encoding of the possible Options available with that label
-- | * Options: Symbols a cell can have (typically numbers 1-9)
-- | * Forced Cell: A cell for which only one possible option 
-- |      remains
-- | * Group: Predefined collection of cells (row, column, or box).
-- | * Row/Col/Box: Names for the 3 groups of a typical sudoku board
-- | * Peers: cells that share a group
-- |
module Sudoku.Board
  ( -- Types
    Board
  , Index
  , Action
    -- Board Constrctors
  , unconstrainedBoard
  , fromCells
  , fromString
    -- Board Operations
  , boardIndex
  , (!!)
  , mapBoard
  , filterIndices
  , findIndex
    -- Board Predicates
  , allCellsValid
  , noForcedPeerDuplicates
  , isValid
  , isSolved
  , isSolvedOrInvalid
  , effective
    -- Index Constructors
  , boundedIndex
  , indexedCells
  , boardIndices
  , indicesRow
  , indicesCol
  , indicesBox
  , indicesPeers
  , indicesExPeers
  , allIndicesRow
  , allIndicesCol
  , allIndicesBox
  , allIndicesGroup
    -- Index Mappings
  , toCol
  , toRow
  , toBox
    -- Index Conversions
  , toInt
  , toNumber
    -- Update Board State
  , batchDropOptions
  , modifyAtIndex
  )
where

import Prelude

import Control.MonadZero (guard)
import Data.Array (all, any, filter, length, mapWithIndex, nub, unsafeIndex, (..))
import Data.Array as Array
import Data.Either (Either, note)
import Data.Int as Ints
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Error (Error(..))
import Math (sqrt)
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import Sudoku.Cell (allOptionsCell, firstOption, isForced, notDisjoint, toCell)
import Sudoku.Cell as Cells
import Sudoku.Cell.Internal (Cell(..))
import Sudoku.Option (boundedOption, numOfOptions)
import Utility (allUniqueEq, both, dropMaskPerIndex)



-------------------------------------------------------------------
-- Types
-------------------------------------------------------------------

newtype Board = Board (Array Cell)

derive newtype instance eqBoard :: Eq Board
derive newtype instance showBoard :: Show Board

newtype Index = Index Int

derive newtype instance eqIndex :: Eq Index
derive newtype instance ordIndex :: Ord Index
derive newtype instance showIndex :: Show Index

type Action = Tuple Index Cell

-------------------------------------------------------------------
-- Invariant
-------------------------------------------------------------------

boardSize :: Int
boardSize = numOfOptions

boardRoot :: Int
boardRoot = Ints.floor $ sqrt $ Ints.toNumber boardSize

-------------------------------------------------------------------
-- Smart Constructors for Boards
-------------------------------------------------------------------

-- | A board where every cell still has every option as a possibility
unconstrainedBoard :: Board
unconstrainedBoard = coerce $ const allOptionsCell <$> 1 .. (boardSize * boardSize)

-- | Attempt to construct a board for an array of cells
fromCells :: Array Cell -> Maybe Board
fromCells cells = if all Cells.isValid cells && length cells == boardSize * boardSize 
  then Just $ coerce cells
  else Nothing

-- | Parsing a board from a string
-- |
-- | For the textual format, we allow a string of characters with 1-9 
-- | indicating a digit, and a 0 or period specifying an empty cell. 
-- | All other characters are ignored. This includes spaces, newlines, 
-- | dashes, and bars
fromString :: String -> Either Error Board
fromString = split (Pattern "") 
  >>> filter (\v -> any (eq v) keepVals) 
  >>> map (Ints.fromString >>> fromMaybe 0)
  >>> (\ints -> note  
    (Error "Parsing Sudoku Board"
    "Input string contained invalid number of characters")
    $ fromCells $ makeStarterCell <$> ints)
  where
    keepVals = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."]
    fullLength = boardSize * boardSize
    makeStarterCell :: Int -> Cell
    makeStarterCell n
      | n > 0 && n <= boardSize = toCell $ boundedOption n
      | otherwise = allOptionsCell

-------------------------------------------------------------------
-- Board operations
-------------------------------------------------------------------

-- | Retrieve a cell from the board. This can use unsafeIndex on the board's
-- | underlying array implemenation since boards (by constrction) always have
-- | the same shape.
boardIndex :: Board -> Index -> Cell
boardIndex (Board array) index = unsafePartial (unsafeIndex array (coerce index))

-- | infix operator for boardIndex
infix 8 boardIndex as !!

-- | Turn a board into an array by applying a function to each cell
mapBoard :: forall a. (Cell -> a) -> Board -> Array a
mapBoard fn (Board array) = fn <$> array

-- | Returns a new array of indices where all index's corrospoding cell
-- | meet some predicate
filterIndices :: (Cell -> Boolean) -> Board -> Array Index -> Array Index
filterIndices pred board indices = filter (\i -> pred $ board !! i) indices

-- | Find the first Index of a Cell that meets some predicate
findIndex :: (Cell -> Boolean) -> Board -> Maybe Index
findIndex pred (Board array) = Index <$> Array.findIndex pred array

-------------------------------------------------------------------
-- Predicates for Boards
-------------------------------------------------------------------

-- | Check if every cell in a board has at least 1 option that is 
-- | still possible
allCellsValid :: Board -> Boolean
allCellsValid (Board array) = all Cells.isValid array

-- | Check that no group has two singleton Cells with the same option
noForcedPeerDuplicates :: Board -> Boolean
noForcedPeerDuplicates board = all allUniqueEq do
  group <- allIndicesGroup
  do
    i <- group
    guard $ isForced $ board !! i
    pure case firstOption $ board !! i of
      Nothing -> []
      Just option -> pure option

-- | Check that a board has both allCellsValid and noForcedPeerDuplicates
isValid :: Board -> Boolean
isValid = both allCellsValid noForcedPeerDuplicates

-- | Check if a board has been solved
isSolved :: Board -> Boolean
isSolved board@(Board array) = 
  all isForced array && isValid board

-- | Check if a board has been solved or is not valid
isSolvedOrInvalid :: Board -> Boolean
isSolvedOrInvalid board@(Board array) = 
  (not (isValid board)) || (all isForced array)

-- | Check if a given action will change a board if applied
effective :: Board -> Action -> Boolean
effective board (Tuple i action) = 
  notDisjoint action $ board !! i

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

-- | Converts a board into an array of indices and cells
indexedCells :: Board -> Array (Tuple Index Cell)
indexedCells (Board array) = mapWithIndex (\i c -> Tuple (Index i) c) array

-- | An Array containing every valid index for a board
boardIndices :: Array Index
boardIndices = coerce $ 0 .. (boardSize * boardSize - 1)

-- | Helper function that ensures a valid group is indicated by an Int
-- | If not, it uses the nearest valid group
boundedGroup :: (Int -> Array Index) -> Int -> Array Index
boundedGroup fn int = 
  if int < 0 
  then fn 0
  else if int >= boardSize 
  then fn $ boardSize -1
  else fn int

-- | An Array containing the indices for the given row
indicesRow :: Int -> Array Index
indicesRow = boundedGroup indices
  where
    indices n = coerce $ (n * boardSize) .. (n * boardSize + boardSize - 1)

-- | An Array containing the indices for the given column
indicesCol :: Int -> Array Index
indicesCol = boundedGroup indices
  where
    indices n = coerce $ (_ * boardSize) >>> (_ + n) <$> 0 .. (boardSize - 1)

-- | An Array containing the indices for the given box
indicesBox :: Int -> Array Index
indicesBox = boundedGroup indices
  where 
    indices n = coerce $ boxIndex <$> boxRange <*> boxRange
      where
        boxRange :: Array Int
        boxRange = 0 .. (boardRoot - 1)

        boxOffset :: Int
        boxOffset = n / boardRoot * boardRoot * boardSize +
          (n `mod` boardRoot) * boardRoot

        boxIndex :: Int -> Int -> Int
        boxIndex r c = boxOffset + boardSize * r + c

-- | An Array containing the indices for the peers of a given index
-- | including itself in the list of peers
indicesPeers :: Index -> Array Index
indicesPeers i = nub $
  (indicesRow $ toRow i) <>
  (indicesCol $ toCol i) <>
  (indicesBox $ toBox i)

-- | An Array containing the indices for the peers of a given index
-- | excluding itself in the list of peers
indicesExPeers :: Index -> Array Index
indicesExPeers i = filter (notEq i) $ indicesPeers i

-- | An array of every row of indices
allIndicesRow :: Array (Array Index)
allIndicesRow = indicesRow <$> 0 .. (numOfOptions - 1)

-- | An array of every column of indices
allIndicesCol :: Array (Array Index)
allIndicesCol = indicesCol <$> 0 .. (numOfOptions - 1)

-- | An array of every box of indices
allIndicesBox :: Array (Array Index)
allIndicesBox = indicesBox <$> 0 .. (numOfOptions - 1)

-- | An array of every group of indices, note that every index will
-- | appear once in each type of group
allIndicesGroup :: Array (Array Index)
allIndicesGroup =
  allIndicesRow <>
  allIndicesCol <>
  allIndicesBox

-------------------------------------------------------------------
-- Index mappings
-- Index -> Row Column & Box
-------------------------------------------------------------------

-- | Returns the column this index is a part of
toCol :: Index -> Int
toCol i = (coerce i) `mod` boardSize

-- | Returns the row this index is a part of
toRow :: Index -> Int
toRow i = (coerce i) / boardSize

-- | Returns the box this index is a part of
toBox :: Index -> Int
toBox index = boardRoot *
  ((i / boardSize) / boardRoot) +
  ((i / boardRoot) `mod` boardRoot)
  where
    i = coerce index

-------------------------------------------------------------------
-- Convert from Index
-------------------------------------------------------------------

-- | Convert index to an Int
toInt :: Index -> Int
toInt = coerce

-- | Convert index to a Number
toNumber :: Index -> Number
toNumber = coerce Ints.toNumber

-------------------------------------------------------------------
-- Update Board State
-------------------------------------------------------------------

-- | Applies an array of Actions to a board
batchDropOptions :: Array Action -> Board -> Board
batchDropOptions = coerce dropMaskPerIndex

-- | Modify a specific index of a board
modifyAtIndex :: (Cell -> Cell) -> Index -> Board -> Board
modifyAtIndex fn i (Board array) = coerce $ Array.modifyAtIndices [toInt i] fn array