-- | A Cell is a set of options. It encodes the options that are still
-- | possible at a given position in a Sudoku Board. 
-- |
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
import Data.Int.Bits as Bi
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe, fromMaybe)
import Safe.Coerce (coerce)
import Sudoku.Cell.Internal (Cell(..), allOptionsInt, bNot, countOptionsLookupTable, (.&.), (.^.), (.|.))
import Sudoku.Cell.Internal (Cell) as CellType
import Sudoku.Option (Option, allOptions, indexOf, invalidOption, numOfOptions)

-------------------------------------------------------------------
-- Smart Constructors for Cells
-------------------------------------------------------------------

-- | The cell where all options are still possible
allOptionsCell :: Cell
allOptionsCell = Cell allOptionsInt

-- | All combinations of possible cells. They have a loose ordering
-- | such that cells with fewer possible options are earlier in the list
-- | (So the last cell is guarnteed to be allOptionsCell)
allCells :: List Cell
allCells = fromFoldable do
  i <- 1 .. numOfOptions
  cellsOfSize i

-- | Return all the combinations of possible cells that have exactly 
-- | N possible options remaining
cellsOfSize :: Int -> Array Cell
cellsOfSize n = filter (countOptions >>> (_ == n)) $ coerce $ 1 .. allOptionsInt

-------------------------------------------------------------------
-- Cell Operations
-------------------------------------------------------------------

-- | Encode an Option as a Cell with one possible option 
toCell :: Option -> Cell
toCell n = Cell $ 1 `Bi.shl` indexOf n

-- | Return a Cell where the given option is guarenteed to be one of the possible
-- | options remaining
setOption :: Option -> Cell -> Cell
setOption = toCell >>> setOptions

-- | Return a Cell where every possible option in the first cell is guarenteed 
-- | to be one of the possible options remaining
setOptions :: Cell -> Cell -> Cell
setOptions = (.|.)

-- | Return a Cell where the given option is guarenteed not to be one of the possible
-- | options remaining
dropOption :: Option -> Cell -> Cell
dropOption = toCell >>> dropOptions

-- | Return a Cell where every possible option in the first cell is guarenteed 
-- | not to be one of the possible options remaining
dropOptions :: Cell -> Cell -> Cell
dropOptions options cell = bNot options .&. cell

-- | Return a Cell where the given option is guarenteed to be different
toggleOption :: Option -> Cell -> Cell
toggleOption = toCell >>> toggleOptions

-- | Return a Cell where every possible option in the first cell is guarenteed 
-- | to be different
toggleOptions :: Cell -> Cell -> Cell
toggleOptions = (.^.)

-- | Return a Cell where every Option is different
toggleCell :: Cell -> Cell
toggleCell = bNot

-- | return an array of Options where each Option was a possible option
-- | in the given Cell
asOptions :: Cell -> Array Option
asOptions cell = filter (flip hasOption $ cell) allOptions

-- | Try to return the first possible option of a cell
firstOption :: Cell -> Maybe Option
firstOption cell = find (flip hasOption cell) allOptions

-- | Like firstOption, but might return invalidOption
-- | Generally best used of countOptions was used first
trustFirstOption :: Cell -> Option
trustFirstOption = firstOption >>> fromMaybe invalidOption

-- | Return the number of options available for this cell
countOptions :: Cell -> Int
countOptions cell = fromMaybe 0 $ countOptionsLookupTable !! (coerce cell - 1)

-------------------------------------------------------------------
-- Predicates for Cells
-------------------------------------------------------------------

-- | Check if a cell is valid
isValid :: Cell -> Boolean
isValid cell = n > 0 && n <= allOptionsInt
  where
    n :: Int
    n = coerce cell

-- | Check if a cell as an option
hasOption :: Option -> Cell -> Boolean
hasOption = toCell >>> notDisjoint

-- | Treat Cells as sets of options
-- | Check if the union of two cells is inhabited
notDisjoint :: Cell -> Cell -> Boolean
notDisjoint a b = a .&. b /= Cell 0

-- | Treat Cells as sets of options
-- | Check if the first Cell is a superset of the second Cell
isSuperset :: Cell -> Cell -> Boolean
isSuperset a b = a .|. b == a

-- | Treat Cells as sets of options
-- | Check if the first Cell is a subset of the second Cell
isSubset :: Cell -> Cell -> Boolean
isSubset = flip isSuperset

-- | Check if a cell has only one possible option (This cell is forced or solved)
isForced :: Cell -> Boolean
isForced = countOptions >>> eq 1