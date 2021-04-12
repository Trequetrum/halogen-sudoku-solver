-- | Cells are encodings of the possible options.
-- |
-- | That cells are implemented as bitfields is an internal detail and subject
-- | to change in the future.
-- | 
module Sudoku.Cell.Internal where

import Prelude

import Data.Array ((..))
import Data.Int (even)
import Data.Int.Bits as Bi
import Safe.Coerce (coerce)
import Sudoku.Option (numOfOptions)

newtype Cell = Cell Int

derive newtype instance eqCell :: Eq Cell
derive newtype instance showCell :: Show Cell

allOptionsInt :: Int
allOptionsInt = (1 `Bi.shl` numOfOptions) - 1

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