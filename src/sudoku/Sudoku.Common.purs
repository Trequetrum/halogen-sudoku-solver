module Sudoku.Common where

import Prelude

import Data.Array (all, filter, find, foldl, nub, (!!), (..))
import Data.Int (even, floor, toNumber)
import Data.Int.Bits as Bi
import Data.List (List, fromFoldable)
import Data.Maybe (fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Math (sqrt)
import Safe.Coerce (coerce)
import Stateful (Stateful)
import Utility (allUniqueEq, both, dropMaskPerIndex, indexOn, unsafeIndexOn)

-------------------- SUDOKU ------------------------------------
-- A sudoku board is an array of cells.
-- 
-- A sudoku strategy takes a board as input and returns a tagged 
-- board as output. 
-- 
-- * Board: Collection of all cells, ordered.
-- * Cell: A single position on a sudoku board. Also; a bitfield
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

newtype Option = Option Int
newtype Cell = Cell Int

derive newtype instance eqOption :: Eq Option
derive newtype instance showOption :: Show Option
derive newtype instance eqCell :: Eq Cell
derive newtype instance showCell :: Show Cell

type Index = Int

type Board = Array Cell
type Group = Array Index

type MetaBoard = 
  { metaData :: {}
  , metaBoard :: (Array {})
  }
type Puzzle = Tuple MetaBoard Board 

type Strategy = Puzzle -> Stateful Puzzle
type StatefulStrategy = Stateful Puzzle -> Stateful Puzzle

type Action = Tuple Index Cell

-------------------------------------------------------------------
-- Some Invariants
-------------------------------------------------------------------
boardSize :: Int
boardSize = 9

boardRoot :: Int
boardRoot = floor $ sqrt $ toNumber boardSize

allOptions :: Array Option
allOptions = coerce $ 1 .. boardSize

allOptionsInt :: Int
allOptionsInt = foldl (+) 0 (coerce $ toCell <$> allOptions)

allOptionsCell :: Cell
allOptionsCell = Cell allOptionsInt

allOptionsCells :: Array Cell
allOptionsCells = coerce $ 1 .. allOptionsInt

-------------------------------------------------------------------
-- Binary Operations 
-- (Treat Cells as Bitmaps)
-------------------------------------------------------------------

bitwiseCellAnd :: Cell -> Cell -> Cell
bitwiseCellAnd = coerce Bi.and

infixl 10 bitwiseCellAnd as .&.

bitwiseCellOr :: Cell -> Cell -> Cell
bitwiseCellOr = coerce Bi.or

infixl 10 bitwiseCellOr as .|.

bitwiseCellShiftRight :: Cell -> Int -> Cell
bitwiseCellShiftRight = coerce Bi.shr

infixl 10 bitwiseCellShiftRight as .>>.

bitwiseCellShiftLeft :: Cell -> Int -> Cell
bitwiseCellShiftLeft = coerce Bi.shl

infixl 10 bitwiseCellShiftLeft as .<<.

bitwiseCellXOr :: Cell -> Cell -> Cell
bitwiseCellXOr = coerce Bi.xor

infixl 10 bitwiseCellXOr as .^.

bitwiseCellComplement :: Cell -> Cell
bitwiseCellComplement = coerce Bi.complement

bNot :: Cell -> Cell
bNot = bitwiseCellComplement

-------------------------------------------------------------------
-- Cell Operations
-------------------------------------------------------------------

toCell :: Option -> Cell
toCell n = Cell $ 1 `Bi.shl` (coerce n - 1)

setOption :: Option -> Cell -> Cell
setOption = toCell >>> setOptions

setOptions :: Cell -> Cell -> Cell
setOptions = (.|.)

dropOption :: Option -> Cell -> Cell
dropOption = toCell >>> dropOptions

dropOptions :: Cell -> Cell -> Cell
dropOptions options cell = bNot options .&. cell

toggleOptions :: Cell -> Cell -> Cell
toggleOptions = (.^.)

asOptions :: Cell -> Array Option
asOptions cell = filter (flip hasOption $ cell) allOptions

firstOption :: Cell -> Option
firstOption cell = fromMaybe (Option 0) $ find (flip hasOption cell) allOptions

countOptions' :: Cell -> Int
countOptions' = counter 0 0
  where
    counter :: Int -> Int -> Cell -> Int
    counter iter acc cell
      | iter == boardSize = acc
      | otherwise = counter
        (iter + 1)
        (acc + (evalPairty $ coerce cell))
        (cell .>>. 1)
    evalPairty :: Int -> Int
    evalPairty n
      | even n = 0
      | otherwise = 1

countOptionsLookupTable :: Array Int
countOptionsLookupTable = countOptions' <$> allOptionsCells

countOptions :: Cell -> Int
countOptions cell = fromMaybe 0 $ countOptionsLookupTable !! (coerce cell - 1)

-------------------------------------------------------------------
-- Common Sudoku Predicates
-------------------------------------------------------------------

hasOption :: Option -> Cell -> Boolean
hasOption = toCell >>> notDisjoint

notDisjoint :: Cell -> Cell -> Boolean
notDisjoint a b = a .&. b /= Cell 0

isSuperset :: Cell -> Cell -> Boolean
isSuperset a b = a .|. b == a

isSubset :: Cell -> Cell -> Boolean
isSubset = flip isSuperset

isForced :: Cell -> Boolean
isForced = countOptions >>> eq 1

allCellsValid :: Board -> Boolean
allCellsValid board = all (both (_ > 0) (_ <= allOptionsInt)) (coerce board)

-- Check that no group has two singleton Cells with the same option
noForcedPeerDuplicates :: Board -> Boolean
noForcedPeerDuplicates board = all allUniqueEq $
  filter(indexOn isForced board >>> fromMaybe false) >>>
  map (unsafeIndexOn firstOption board)
  <$> allIndicesGroup

isValid :: Board -> Boolean
isValid = both allCellsValid noForcedPeerDuplicates

isSolved :: Board -> Boolean
isSolved board = all isForced board && isValid board

isSolvedOrInvalid :: Board -> Boolean
isSolvedOrInvalid board = (not (isValid board)) || (all isForced board)

repChange :: Board -> Action -> Boolean
repChange board (Tuple i action) = 
  maybe false (notDisjoint action) $ board !! i
  
-------------------------------------------------------------------
-- get
-------------------------------------------------------------------

cellsOfSize :: Int -> Array Cell
cellsOfSize n = filter (countOptions >>> (_ == n)) allOptionsCells

cellsBySize :: List Cell
cellsBySize = fromFoldable do
  i <- 1 .. boardSize
  cellsOfSize i

-------------------------------------------------------------------
-- Index -> Row Column & Box
-------------------------------------------------------------------

toCol :: Index -> Int
toCol = (_ `mod` boardSize)

toRow :: Index -> Int
toRow = (_ / boardSize)

toBox :: Index -> Int
toBox i = boardRoot *
  ((i / boardSize) / boardRoot) +
  ((i / boardRoot) `mod` boardRoot)

-------------------------------------------------------------------
-- Get Indices
-------------------------------------------------------------------

boardIndices :: Array Index
boardIndices = 0 .. (boardSize * boardSize - 1)

basicGroup :: Array Index
basicGroup = 0 .. (boardSize - 1)

indicesRow :: Int -> Array Index
indicesRow n = (n * boardSize) .. (n * boardSize + boardSize - 1)

indicesCol :: Int -> Array Index
indicesCol n = (_ * boardSize) >>> (_ + n) <$> basicGroup

indicesBox :: Int -> Array Index
indicesBox n = boxIndex <$> boxRange <*> boxRange
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
allIndicesRow = indicesRow <$> basicGroup

allIndicesCol :: Array (Array Index)
allIndicesCol = indicesCol <$> basicGroup

allIndicesBox :: Array (Array Index)
allIndicesBox = indicesBox <$> basicGroup

allIndicesGroup :: Array (Array Index)
allIndicesGroup =
  allIndicesRow <>
  allIndicesCol <>
  allIndicesBox

-------------------------------------------------------------------
-- Update Board State
-------------------------------------------------------------------

batchDropOptions :: Array Action -> Board -> Board
batchDropOptions = coerce dropMaskPerIndex
