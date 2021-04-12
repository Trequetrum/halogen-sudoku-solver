-- | Options are the symbols that appear on a sudoku board.
-- | Typically these are the numbers 1 - 9, but that's not nessesary
-- |
-- | Options have an ordering so that they can be consistently referenced,
-- | the ordering isn't really important.
module Sudoku.Option
  ( Option
  , numOfOptions
  , invalidOption
  , nthOption
  , boundedOption
  , allOptions
  , asString
  , indexOf
  ) where

import Prelude

import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Safe.Coerce (coerce)

newtype Option = Option Int

derive newtype instance eqOption :: Eq Option
derive newtype instance ordOption :: Ord Option
derive newtype instance showOption :: Show Option

-- | The number of options 
numOfOptions :: Int
numOfOptions = 9

-------------------------------------------------------------------
-- Smart Constructors for Options
-------------------------------------------------------------------

-- | To keep the runtime representation of options small, an invalidOption
-- | exists as the numOfOptions + 1th option. 
invalidOption :: Option
invalidOption = Option 0

-- | Try to create an Option from an Int
nthOption :: Int -> Maybe Option
nthOption n
  | n < 1 = Nothing
  | n > numOfOptions = Nothing
  | otherwise = Just (Option n)

-- | Create an option from an Int, use the nearest available option
boundedOption :: Int -> Option
boundedOption n
  | n < 1 = Option 1
  | n > numOfOptions = Option numOfOptions
  | otherwise = Option n

-- | An array of all the possible options. 
-- | boundedOption 1 == allOptions !! 0
allOptions :: Array Option
allOptions = coerce $ 1 .. numOfOptions

-------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------

-- | Return an Option as a string
asString :: Option -> String
asString (Option 0) = "Ԑ"
asString option = show option

-- | Since options are orderd, you can learn the ordering of an Options
-- | forall (x :: Option). allOptions !! (indexOf x) == x
indexOf :: Option -> Int
indexOf = coerce >>> (_ - 1 )