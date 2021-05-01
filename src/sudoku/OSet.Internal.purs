-- | Cells are encodings of the possible options.
-- |
-- | That cells are implemented as bitfields is an internal detail and subject
-- | to change in the future.
-- | 
module Sudoku.OSet.Internal where

import Prelude

import Data.Array ((..))
import Data.Int (even)
import Data.Int.Bits as Bi
import Safe.Coerce (coerce)
import Sudoku.Option (numOfOptions)

newtype OSet = OSet Int

derive newtype instance eqOSet :: Eq OSet
derive newtype instance showOSet :: Show OSet

allOptionsInt :: Int
allOptionsInt = (1 `Bi.shl` numOfOptions) - 1

countOptions' :: OSet -> Int
countOptions' = counter 0 0
  where
    counter :: Int -> Int -> OSet -> Int
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

bitwiseCellAnd :: OSet -> OSet -> OSet
bitwiseCellAnd = coerce Bi.and

infixl 10 bitwiseCellAnd as .&.

bitwiseCellOr :: OSet -> OSet -> OSet
bitwiseCellOr = coerce Bi.or

infixl 10 bitwiseCellOr as .|.

bitwiseCellShiftRight :: OSet -> Int -> OSet
bitwiseCellShiftRight = coerce Bi.shr

infixl 10 bitwiseCellShiftRight as .>>.

bitwiseCellShiftLeft :: OSet -> Int -> OSet
bitwiseCellShiftLeft = coerce Bi.shl

infixl 10 bitwiseCellShiftLeft as .<<.

bitwiseCellXOr :: OSet -> OSet -> OSet
bitwiseCellXOr = coerce Bi.xor

infixl 10 bitwiseCellXOr as .^.

bitwiseCellComplement :: OSet -> OSet
bitwiseCellComplement = coerce Bi.complement

bNot :: OSet -> OSet
bNot = bitwiseCellComplement