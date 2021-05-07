-- | An OSet is a set of options. It encodes the options that are still
-- | possible at a given position in a Sudoku Board. 
-- |
module Sudoku.OSet 
  ( module SetType

  , allOptionsSet
  , noOptionsSet
  , allSets
  , setsOfSize
  , toOSet

  , setOption
  , setOptions
  , dropOption
  , dropOptions
  , toggleOptions
  , toggleOSet
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

  , asTokenString
  )
where

import Prelude

import Data.Array (filter, find, (!!), (..))
import Data.Int.Bits as Bi
import Data.List (List, fromFoldable, range)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (joinWith)
import Safe.Coerce (coerce)
import Sudoku.OSet.Internal (OSet(..), allOptionsInt, bNot, countOptionsLookupTable, (.&.), (.^.), (.|.))
import Sudoku.OSet.Internal (OSet) as SetType
import Sudoku.Option (Option, allOptions, asString, indexOf, invalidOption, numOfOptions)

-------------------------------------------------------------------
-- Smart Constructors for Cells
-------------------------------------------------------------------

-- | The OSet where all options are still possible
allOptionsSet :: OSet
allOptionsSet = OSet allOptionsInt

-- | The cell where no options are still possible. This is not a valid 
-- | cell in a sudoku board, but it is still a valid combination of options.
noOptionsSet :: OSet
noOptionsSet = OSet 0

-- | All combinations of possible sets of options. They have a loose ordering
-- | such that sets with fewer options are earlier in the list
-- | (So the last set is guarnteed to be allOptionsOSet)
allSets :: List OSet
allSets = range 1 numOfOptions >>= (setsOfSize >>> fromFoldable)

-- | Return all the combinations of possible sets that have exactly 
-- | N options remaining
setsOfSize :: Int -> Array OSet
setsOfSize n = filter (countOptions >>> (_ == n)) $ coerce $ 1 .. allOptionsInt

-------------------------------------------------------------------
-- OSet Operations
-------------------------------------------------------------------

-- | Encode an Option as a set with one possible option 
toOSet :: Option -> OSet
toOSet n = OSet $ 1 `Bi.shl` indexOf n

-- | Return a set where the given option is guarenteed to be one of the possible
-- | options remaining
setOption :: Option -> OSet -> OSet
setOption = toOSet >>> setOptions

-- | Return a set where every possible option in the first set is guarenteed 
-- | to be one of the possible options remaining
setOptions :: OSet -> OSet -> OSet
setOptions = (.|.)

-- | Return a set where the given option is guarenteed not to be one of the possible
-- | options remaining
dropOption :: Option -> OSet -> OSet
dropOption = toOSet >>> dropOptions

-- | Return a set where every possible option in the first set is guarenteed 
-- | not to be one of the possible options remaining
dropOptions :: OSet -> OSet -> OSet
dropOptions options set = bNot options .&. set

-- | Return a set where the given option is guarenteed to be different
toggleOption :: Option -> OSet -> OSet
toggleOption = toOSet >>> toggleOptions

-- | Return a set where every possible option in the first set is guarenteed 
-- | to be different if it's in the second set
toggleOptions :: OSet -> OSet -> OSet
toggleOptions = (.^.)

-- | Return a set where every Option is different (It's complement)
toggleOSet :: OSet -> OSet
toggleOSet = bNot

-- | return an array of Options where each Option is an element of the set
asOptions :: OSet -> Array Option
asOptions set = filter (flip hasOption $ set) allOptions

-- | Try to return the first possible option of a cell
firstOption :: OSet -> Maybe Option
firstOption set = find (flip hasOption set) allOptions

-- | Like firstOption, but might return invalidOption
-- | Generally best used of countOptions was used first
trustFirstOption :: OSet -> Option
trustFirstOption = firstOption >>> fromMaybe invalidOption

-- | Return the number of options available for this cell
countOptions :: OSet -> Int
countOptions set = fromMaybe 0 $ countOptionsLookupTable !! (coerce set - 1)

-------------------------------------------------------------------
-- Predicates for Cells
-------------------------------------------------------------------

-- | Check if a set is valid
-- |   * It has elements (not the empty set)
-- |   * All its elements are options
isValid :: OSet -> Boolean
isValid set = n > 0 && n <= allOptionsInt
  where
    n :: Int
    n = coerce set

-- | Check if a set has an option
hasOption :: Option -> OSet -> Boolean
hasOption = toOSet >>> notDisjoint

-- | Check if the union of two sets is inhabited
notDisjoint :: OSet -> OSet -> Boolean
notDisjoint a b = a .&. b /= OSet 0

-- | Check if the first set is a superset of the second set
isSuperset :: OSet -> OSet -> Boolean
isSuperset a b = a .|. b == a

-- | Check if the first set is a subset of the second set
isSubset :: OSet -> OSet -> Boolean
isSubset = flip isSuperset

-- | Check if a set has only one possible option (This set is forced or solved)
isForced :: OSet -> Boolean
isForced = countOptions >>> eq 1

-------------------------------------------------------------------
-- Formatting for Cells
-------------------------------------------------------------------

-- | A string with each option in a set
-- | For example allOptionsSet ({1,2,3,4,5,6,7,8,9}) == "123456789"
asTokenString :: OSet -> String
asTokenString set = let 
  mapOption opt = if hasOption opt set 
    then asString opt 
    else ""
  in joinWith "" $ mapOption <$> allOptions