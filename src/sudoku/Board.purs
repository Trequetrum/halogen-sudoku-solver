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
  ( -- Invariants
    boardSize 
  , boardRoot
    -- Types
  , Board
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
  , allCellsValid'
  , noForcedPeerDuplicates
  , noForcedPeerDuplicates'
  , isValid
  , isValid'
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

import Control.Bind (bindFlipped)
import Control.Alternative (guard)
import Data.Array (all, any, filter, foldl, length, mapMaybe, mapWithIndex, unsafeIndex, (..))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Foldable (oneOf)
import Data.Int as Ints
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Error (Error(..))
import Data.Number (sqrt)
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import Sudoku.OSet (allOptionsSet, asTokenString, firstOption, isForced, notDisjoint, toOSet)
import Sudoku.OSet as OSets
import Sudoku.OSet.Internal (OSet(..))
import Sudoku.Group (Group, asIdString, groupIndices, groups)
import Sudoku.Index (boundedIndex, toInt)
import Sudoku.Index.Internal (Index(..))
import Sudoku.Option (Option, asString, boundedOption, invalidOption, numOfOptions)
import Utility (allUniqueEq, both, dropMaskPerIndex, justWhen)

-------------------------------------------------------------------
-- Types
-------------------------------------------------------------------

newtype Board = Board (Array OSet)

derive newtype instance eqBoard :: Eq Board
derive newtype instance showBoard :: Show Board

type Action = Tuple Index OSet

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
unconstrainedBoard = coerce $ const allOptionsSet <$> 1 .. (boardSize * boardSize)

-- | Attempt to construct a board from an array of cells
fromCells :: Array OSet -> Maybe Board
fromCells cells = if all OSets.isValid cells && length cells == boardSize * boardSize 
  then Just $ coerce cells
  else Nothing

-- | Parsing a board from a string
-- |
-- | For the textual format, we allow a string of characters with 1-9 
-- | indicating a digit, and a 0 or period specifying an empty cell. 
-- | All other characters are ignored. This includes spaces, newlines, 
-- | dashes, and bars
fromString :: String -> Error \/ Board
fromString = split (Pattern "") 
  >>> filter (\v -> any (eq v) keepVals) 
  >>> map (Ints.fromString >>> fromMaybe 0)
  >>> (\ints -> note  
    (Error "Parsing Sudoku Board"
    "Input string contained invalid number of characters")
    $ fromCells $ makeStarterCell <$> ints)
  where
    keepVals :: Array String
    keepVals = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."]

    makeStarterCell :: Int -> OSet
    makeStarterCell n
      | n > 0 && n <= boardSize = toOSet $ boundedOption (n - 1)
      | otherwise = allOptionsSet

-------------------------------------------------------------------
-- Board operations
-------------------------------------------------------------------

-- | Retrieve a set from the board. This can use unsafeIndex on the board's
-- | underlying array implemenation since boards (by constrction) always have
-- | the same shape.
boardIndex :: Board -> Index -> OSet
boardIndex (Board array) index = unsafePartial (unsafeIndex array (toInt index))

-- | infix operator for boardIndex
infix 8 boardIndex as !!

-- | Turn a board into an array by applying a function to each cell
mapBoard :: forall a. (OSet -> a) -> Board -> Array a
mapBoard fn (Board array) = fn <$> array

-- | Returns a new array of indices where all index's corrospoding sets
-- | meet some predicate
filterIndices :: (OSet -> Boolean) -> Board -> Array Index -> Array Index
filterIndices pred board indices = filter (\i -> pred $ board !! i) indices

-- | Find the first Index of a set that meets some predicate
findIndex :: (OSet -> Boolean) -> Board -> Maybe Index
findIndex pred (Board array) = boundedIndex <$> Array.findIndex pred array

-- | Return the first Cell (Index & set) in the board that meets some predicate
find :: (OSet -> Boolean) -> Board -> Maybe (Tuple Index OSet)
find pred board = case findIndex pred board of
  Nothing -> Nothing
  (Just index) -> Just $ Tuple index $ board !! index

-- | Converts a board into an array of indices and cells
indexedCells :: Board -> Array (Tuple Index OSet)
indexedCells (Board array) = mapWithIndex (\i c -> Tuple (boundedIndex i) c) array

-------------------------------------------------------------------
-- Predicates for Boards
-------------------------------------------------------------------

-- | Check if every set in a board has at least 1 option that is 
-- | still possible
allCellsValid :: Board -> Boolean
allCellsValid (Board array) = all OSets.isValid array

-- | A version of allCellsValid that returns an error instead of false
allCellsValid' :: Board -> Maybe Error
allCellsValid' board = intoError <$> find (not <<< OSets.isValid) board
  where
    intoError (Tuple index set) = Error "Invalid Cell" ("Board Position " <> show index <> 
      " contains invalid set with option(s): (" <> asTokenString set <> ")")

-- | Check that no group has two singleton sets with the same option
noForcedPeerDuplicates :: Board -> Boolean
noForcedPeerDuplicates board = all allUniqueEq do
  group <- groups
  pure do 
    i <- groupIndices group
    guard $ isForced $ board !! i
    case firstOption $ board !! i of
      Nothing -> []
      Just option -> pure option

-- | Check that this group does not have two singleton Cells with the same option
noForcedDuplicates :: Board -> Group -> Maybe Error
noForcedDuplicates board group = 
  case foldl folder (Right invalidOption) singletons of
    Left err -> Just err
    _ -> Nothing
  where
    singletons :: Array Option 
    singletons = mapMaybe 
      (boardIndex board >>> justWhen isForced >>> bindFlipped firstOption) 
      (groupIndices group)
    
    folder :: Error \/ Option -> Option -> Error \/ Option
    folder (Right option) nOption = 
      if option /= nOption
      then Right nOption
      else Left $ Error "Duplicate Forced Options" (asIdString group <> 
        " has two cells with option '" <> 
        asString option <> "'")
    folder leftError _ = leftError

-- | A version of noForcedPeerDuplicates that returns an error instead of false
noForcedPeerDuplicates' :: Board -> Maybe Error
noForcedPeerDuplicates' board = oneOf $ noForcedDuplicates board <$> groups

-- | Check that a board has both allCellsValid and noForcedPeerDuplicates
isValid :: Board -> Boolean
isValid = both allCellsValid noForcedPeerDuplicates

-- | A version of isValid that returns an error instead of false
isValid' :: Board -> Maybe Error
isValid' board = case (allCellsValid' board), (noForcedPeerDuplicates' board) of
  Nothing, Nothing -> Nothing
  _, err@(Just _) -> err
  err@(Just _), Nothing -> err

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
modifyAtIndex :: (OSet -> OSet) -> Index -> Board -> Board
modifyAtIndex fn i (Board array) = coerce $ Array.modifyAtIndices [toInt i] fn array