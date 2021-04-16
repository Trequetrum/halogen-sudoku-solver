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
  , indexedCells
    -- Board Predicates
  , allCellsValid
  , noForcedPeerDuplicates
  , isValid
  , isSolved
  , isSolvedIffValid
  , isSolvedOrInvalid
  , effective
    -- Update Board State
  , batchDropOptions
  , modifyAtIndex
  )
where

import Prelude

import Control.MonadZero (guard)
import Data.Array (all, any, filter, length, mapWithIndex, unsafeIndex, (..))
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
import Sudoku.Group (groupIndices, groups)
import Sudoku.Index (boundedIndex, toInt)
import Sudoku.Index.Internal (Index(..))
import Sudoku.Option (boundedOption, numOfOptions)
import Utility (allUniqueEq, both, dropMaskPerIndex)

-------------------------------------------------------------------
-- Types
-------------------------------------------------------------------

newtype Board = Board (Array Cell)

derive newtype instance eqBoard :: Eq Board
derive newtype instance showBoard :: Show Board

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
      | n > 0 && n <= boardSize = toCell $ boundedOption (n - 1)
      | otherwise = allOptionsCell

-------------------------------------------------------------------
-- Board operations
-------------------------------------------------------------------

-- | Retrieve a cell from the board. This can use unsafeIndex on the board's
-- | underlying array implemenation since boards (by constrction) always have
-- | the same shape.
boardIndex :: Board -> Index -> Cell
boardIndex (Board array) index = unsafePartial (unsafeIndex array (toInt index))

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
findIndex pred (Board array) = boundedIndex <$> Array.findIndex pred array

-- | Converts a board into an array of indices and cells
indexedCells :: Board -> Array (Tuple Index Cell)
indexedCells (Board array) = mapWithIndex (\i c -> Tuple (boundedIndex i) c) array

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
  group <- groups
  pure do 
    i <- groupIndices group
    guard $ isForced $ board !! i
    case firstOption $ board !! i of
      Nothing -> []
      Just option -> pure option

-- | Check that a board has both allCellsValid and noForcedPeerDuplicates
isValid :: Board -> Boolean
isValid = both allCellsValid noForcedPeerDuplicates

-- | Check if a board has been solved
isSolved :: Board -> Boolean
isSolved board@(Board array) = 
  all isForced array && isValid board

isSolvedIffValid :: Board -> Boolean
isSolvedIffValid (Board array) =
  all isForced array

-- | Check if a board has been solved or is not valid
isSolvedOrInvalid :: Board -> Boolean
isSolvedOrInvalid board@(Board array) = 
  (not (isValid board)) || (all isForced array)

-- | Check if a given action will change a board if applied
effective :: Board -> Action -> Boolean
effective board (Tuple i action) = 
  notDisjoint action $ board !! i

-------------------------------------------------------------------
-- Update Board State
-------------------------------------------------------------------

-- | Applies an array of Actions to a board
batchDropOptions :: Array Action -> Board -> Board
batchDropOptions = coerce dropMaskPerIndex

-- | Modify a specific index of a board
modifyAtIndex :: (Cell -> Cell) -> Index -> Board -> Board
modifyAtIndex fn i (Board array) = coerce $ Array.modifyAtIndices [toInt i] fn array