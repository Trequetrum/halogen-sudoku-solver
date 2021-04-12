module Sudoku.Cell 
  (module CellType

  , allOptionsCell
  , allCells
  , cellsOfSize
  , toCell

  , setOption
  , setOptions
  , dropOption
  , dropOptions
  , toggleOptions
  , toggleCell
  , asOptions
  , firstOption
  , trustFirstOption
  , countOptions

  , isValid
  , hasOption
  , notDisjoint
  , isSuperset
  , isSubset
  , isForced
  )
where

import Prelude

import Data.Array (filter, find, (!!), (..))
import Data.Int (even)
import Data.Int.Bits as Bi
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe, fromMaybe)
import Safe.Coerce (coerce)
import Sudoku.Cell.Internal (Cell(..), allOptionsInt, bNot, (.&.), (.>>.), (.^.), (.|.))
import Sudoku.Cell.Internal (Cell) as CellType
import Sudoku.Option (Option, allOptions, indexOf, invalidOption, numOfOptions)

-------------------------------------------------------------------
-- Smart Constructors for Cells
-------------------------------------------------------------------

allOptionsCell :: Cell
allOptionsCell = Cell allOptionsInt

allCells :: List Cell
allCells = fromFoldable do
  i <- 1 .. numOfOptions
  cellsOfSize i

cellsOfSize :: Int -> Array Cell
cellsOfSize n = filter (countOptions >>> (_ == n)) $ coerce $ 1 .. allOptionsInt

-------------------------------------------------------------------
-- Cell Operations
-------------------------------------------------------------------

toCell :: Option -> Cell
toCell n = Cell $ 1 `Bi.shl` indexOf n

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

toggleCell :: Cell -> Cell
toggleCell = bNot

asOptions :: Cell -> Array Option
asOptions cell = filter (flip hasOption $ cell) allOptions

firstOption :: Cell -> Maybe Option
firstOption cell = find (flip hasOption cell) allOptions

trustFirstOption :: Cell -> Option
trustFirstOption = firstOption >>> fromMaybe invalidOption

countOptions' :: Cell -> Int
countOptions' = counter 0 0
  where
    counter :: Int -> Int -> Cell -> Int
    counter iter acc cell
      | iter == numOfOptions = acc
      | otherwise = counter
        (iter + 1)
        (acc + (evalPairty $ coerce cell))
        (cell .>>. 1)
    evalPairty :: Int -> Int
    evalPairty n
      | even n = 0
      | otherwise = 1

countOptionsLookupTable :: Array Int
countOptionsLookupTable = countOptions' <$> (coerce $ 1 .. allOptionsInt)

countOptions :: Cell -> Int
countOptions cell = fromMaybe 0 $ countOptionsLookupTable !! (coerce cell - 1)

-------------------------------------------------------------------
-- Predicates for Cells
-------------------------------------------------------------------

isValid :: Cell -> Boolean
isValid cell = n > 0 && n <= allOptionsInt
  where
    n :: Int
    n = coerce cell

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