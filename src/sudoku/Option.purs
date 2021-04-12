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

numOfOptions :: Int
numOfOptions = 9

-------------------------------------------------------------------
-- Smart Constructors for Options
-------------------------------------------------------------------

invalidOption :: Option
invalidOption = Option 0

nthOption :: Int -> Maybe Option
nthOption n
  | n < 1 = Nothing
  | n > numOfOptions = Nothing
  | otherwise = Just (Option n)

boundedOption :: Int -> Option
boundedOption n
  | n < 1 = Option 1
  | n > numOfOptions = Option numOfOptions
  | otherwise = Option n

allOptions :: Array Option
allOptions = coerce $ 1 .. numOfOptions

-------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------

asString :: Option -> String
asString (Option 0) = "Ԑ"
asString option = show option

indexOf :: Option -> Int
indexOf = coerce >>> (_ - 1 )