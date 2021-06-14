-- | An OSet is a set of options. 
-- |
-- | One use of an OSet is to encode the options that are still possible at a given 
-- | position in a Sudoku Board. Some strategies encode information about their
-- | search-space as well.
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
  , toggleOption
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

-- | Every option is an element of this set
allOptionsSet :: OSet
allOptionsSet = OSet allOptionsInt

-- | The empty set
noOptionsSet :: OSet
noOptionsSet = OSet 0

-- | All combinations of possible sets of options. They have a loose ordering
-- | such that sets with fewer options are earlier in the list
-- | (So the last set is guarnteed to be allOptionsOSet)
allSets :: List OSet
allSets = range 1 numOfOptions >>= (setsOfSize >>> fromFoldable)

-- | Return all the sets with the cardinality equal to the input
setsOfSize :: Int -> Array OSet
setsOfSize n = filter (countOptions >>> (_ == n)) $ coerce $ 1 .. allOptionsInt

-------------------------------------------------------------------
-- OSet Operations
-------------------------------------------------------------------

-- | Encode an Option as a set with cardinality 1
toOSet :: Option -> OSet
toOSet n = OSet $ 1 `Bi.shl` indexOf n

-- | Return a set where the given option is guarenteed to be an element of the set
setOption :: Option -> OSet -> OSet
setOption = toOSet >>> setOptions

-- | Return the union of two sets
setOptions :: OSet -> OSet -> OSet
setOptions = (.|.)

-- | Return a set where the given option is guarenteed not to be an element of the set
dropOption :: Option -> OSet -> OSet
dropOption = toOSet >>> dropOptions

-- | Return a set where every element of the first set is guarenteed not to be an 
-- | element of the returned set
dropOptions :: OSet -> OSet -> OSet
dropOptions options set = bNot options .&. set

-- | Return a set where Option is either removed from or added as an element such
toggleOption :: Option -> OSet -> OSet
toggleOption = toOSet >>> toggleOptions

-- | Pairwise XOR (this is the union of differences)
-- | Written as: ( (A - B) U (B - A) )
toggleOptions :: OSet -> OSet -> OSet
toggleOptions = (.^.)

-- | Return the complement of the set.
toggleOSet :: OSet -> OSet
toggleOSet = bNot

-- | return an array of options where each option is an element of the set
asOptions :: OSet -> Array Option
asOptions set = filter (flip hasOption $ set) allOptions

-- | Try to return the first possible option of a set.
firstOption :: OSet -> Maybe Option
firstOption set = find (flip hasOption set) allOptions

-- | Like firstOption, but might return invalidOption
-- | Generally best used if countOptions was used first
trustFirstOption :: OSet -> Option
trustFirstOption = firstOption >>> fromMaybe invalidOption

-- | Return the number of options available for this cell
countOptions :: OSet -> Int
countOptions set = fromMaybe 0 $ countOptionsLookupTable !! (coerce set - 1)

-------------------------------------------------------------------
-- Predicates for OSets
-------------------------------------------------------------------

-- | Check if a set is valid as a cell
-- |   * It has elements (not the empty set)
-- |   * All its elements are options
isValid :: OSet -> Boolean
isValid set = n > 0 && n <= allOptionsInt
  where
    n :: Int
    n = coerce set

-- | Check if an option is an element of a set
hasOption :: Option -> OSet -> Boolean
hasOption = toOSet >>> notDisjoint

-- | Check if the union of two sets is inhabited
notDisjoint :: OSet -> OSet -> Boolean
notDisjoint a b = a .&. b /= OSet 0

-- | Check if the first set is a superset of the second set
-- | This is not strict, so (a `isSuperset` a) == true
isSuperset :: OSet -> OSet -> Boolean
isSuperset a b = a .|. b == a

-- | Check if the first set is a subset of the second set
-- | This is not strict, so (a `isSubset` a) == true
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
