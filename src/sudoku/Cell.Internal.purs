module Sudoku.Cell.Internal where

import Prelude

import Data.Int.Bits as Bi
import Safe.Coerce (coerce)
import Sudoku.Option (numOfOptions)

newtype Cell = Cell Int

derive newtype instance eqCell :: Eq Cell
derive newtype instance showCell :: Show Cell

allOptionsInt :: Int
allOptionsInt = (1 `Bi.shl` numOfOptions) - 1

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