module Sudoku.Board
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

-------------------- SUDOKU ------------------------------------
-- A sudoku board is an array of cells.
-- 
-- A sudoku strategy takes a board as input and returns a tagged 
-- board as output. 
-- 
-- * Board: Collection of all cells, ordered.
-- * Cell: Value of a board at an Index (position). This is an
--      encoding of the possible Options available at that position
-- * Options: Symbols a cell can have (typically numbers 1-9)
-- * Forced Cell: A cell for which only one possible option 
--      remains
-- * Group: Collection of cells (row, column, or box).
-- * Row/Col: Typical cartesian coordinates of a board mapped to 
--      a 2D grid
-- * Peers: cells that share a group
-- * Strategy: A function that takes a board as input and returns 
--      a stateful board as output. It should never add options to 
--      the returned board, (what it does internally doesn't matter). 
-- *# Advancing: A strategy returns an advancing board if it has 
--       removed at least one option from the board.
-- *# Stable: A strategy returns a stable board if it has made no 
--       changes to the board.
-- *# Finished: A strategy returns a finished board if it has 
--       concluded that a board is either solved or impossible to 
--       solve 
--       Because strategies should not add options to a board,
--       any board that is in an illegal/invalid state is finished
-------------------------------------------------------------------

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

type Group = Array Index

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

boardIndex :: Board -> Index -> Cell
boardIndex (Board array) index = unsafePartial (unsafeIndex array (coerce index))

infix 8 boardIndex as !!

mapBoard :: forall a. (Cell -> a) -> Board -> Array a
mapBoard fn (Board array) = fn <$> array

filterIndices :: (Cell -> Boolean) -> Board -> Array Index -> Array Index
filterIndices pred board indices = filter (\i -> pred $ board !! i) indices

findIndex :: (Cell -> Boolean) -> Board -> Maybe Index
findIndex pred (Board array) = Index <$> Array.findIndex pred array

-------------------------------------------------------------------
-- Predicates for Boards
-------------------------------------------------------------------

allCellsValid :: Board -> Boolean
allCellsValid (Board array) = all Cells.isValid array

-- Check that no group has two singleton Cells with the same option
noForcedPeerDuplicates :: Board -> Boolean
noForcedPeerDuplicates board = all allUniqueEq do
  group <- allIndicesGroup
  do
    i <- group
    guard $ isForced $ board !! i
    pure case firstOption $ board !! i of
      Nothing -> []
      Just option -> pure option

isValid :: Board -> Boolean
isValid = both allCellsValid noForcedPeerDuplicates

isSolved :: Board -> Boolean
isSolved board@(Board array) = 
  all isForced array && isValid board

isSolvedOrInvalid :: Board -> Boolean
isSolvedOrInvalid board@(Board array) = 
  (not (isValid board)) || (all isForced array)

repChange :: Board -> Action -> Boolean
repChange board (Tuple i action) = 
  notDisjoint action $ board !! i

-------------------------------------------------------------------
-- Index -> Row Column & Box
-------------------------------------------------------------------

toCol :: Index -> Int
toCol i = (coerce i) `mod` boardSize

toRow :: Index -> Int
toRow i = (coerce i) / boardSize

toBox :: Index -> Int
toBox index = boardRoot *
  ((i / boardSize) / boardRoot) +
  ((i / boardRoot) `mod` boardRoot)
  where
    i = coerce index

-------------------------------------------------------------------
-- Smart Constructors for Arrays of Indices
-------------------------------------------------------------------

boundedIndex :: Int -> Index
boundedIndex n
  | n >= length boardIndices = Index $ length boardIndices - 1
  | n < 0 = Index 0
  | otherwise = Index n

indexedCells :: Board -> Array (Tuple Index Cell)
indexedCells (Board array) = mapWithIndex (\i c -> Tuple (Index i) c) array

boardIndices :: Array Index
boardIndices = coerce $ 0 .. (boardSize * boardSize - 1)

indicesRow :: Int -> Array Index
indicesRow n = coerce $ (n * boardSize) .. (n * boardSize + boardSize - 1)

indicesCol :: Int -> Array Index
indicesCol n = coerce $ (_ * boardSize) >>> (_ + n) <$> 0 .. (boardSize - 1)

indicesBox :: Int -> Array Index
indicesBox n = coerce $ boxIndex <$> boxRange <*> boxRange
  where
    boxRange :: Array Int
    boxRange = 0 .. (boardRoot - 1)

    boxOffset :: Int
    boxOffset = n / boardRoot * boardRoot * boardSize +
      (n `mod` boardRoot) * boardRoot

    boxIndex :: Int -> Int -> Int
    boxIndex r c = boxOffset + boardSize * r + c

indicesPeers :: Index -> Array Index
indicesPeers i = nub $
  (indicesRow $ toRow i) <>
  (indicesCol $ toCol i) <>
  (indicesBox $ toBox i)

indicesExPeers :: Index -> Array Index
indicesExPeers i = filter (notEq i) $ indicesPeers i

allIndicesRow :: Array (Array Index)
allIndicesRow = indicesRow <$> 0 .. (numOfOptions - 1)

allIndicesCol :: Array (Array Index)
allIndicesCol = indicesCol <$> 0 .. (numOfOptions - 1)

allIndicesBox :: Array (Array Index)
allIndicesBox = indicesBox <$> 0 .. (numOfOptions - 1)

allIndicesGroup :: Array (Array Index)
allIndicesGroup =
  allIndicesRow <>
  allIndicesCol <>
  allIndicesBox

-------------------------------------------------------------------
-- Convert from Index
-------------------------------------------------------------------

toInt :: Index -> Int
toInt = coerce

toNumber :: Index -> Number
toNumber = coerce Ints.toNumber

-------------------------------------------------------------------
-- Update Board State
-------------------------------------------------------------------

batchDropOptions :: Array Action -> Board -> Board
batchDropOptions = coerce dropMaskPerIndex

updateAtIndex :: (Cell -> Cell) -> Index -> Board -> Board
updateAtIndex fn i (Board array) = coerce $ Array.modifyAtIndices [toInt i] fn array