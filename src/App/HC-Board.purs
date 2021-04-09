module App.HC.Board where

import Prelude

import App.HC.Cell as HCCell
import App.Update (Update(..))
import Data.Array (concat, mapWithIndex, (..))
import Data.Int (toNumber)
import Data.Tuple (Tuple)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Math ((%))
import Sudoku.Common (Board, Cell, Index, Option, boardRoot, boardSize, toCol, toRow)
import Sudoku.Format (beforeSudokuBorder)
import Type.Prelude (Proxy(..))
import Utility (inc)

type Input = Update Board

data Output = ToggleOn (Tuple Index Option) 
  | ToggleOff (Tuple Index Option) 
  | SetTo (Tuple Index Cell)

type State = Update Board

data Action = Raise Output

type Slots = ( cell :: ∀ query. H.Slot query Cell Int )

_cell = Proxy :: Proxy "cell"

component :: ∀ query output m. 
  H.Component query State output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: ∀ m. State -> H.ComponentHTML Action Slots m
render state = HH.div
  [ HP.classes [ HH.ClassName "ss-board-container" ] ]
  $ concat $ mapWithIndex makeHCell state

handleAction :: ∀ cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction (Raise n) = do
  pure unit

makeHCell :: ∀ m. Index -> Cell -> Array (H.ComponentHTML Action Slots m)
makeHCell i cell =
  [ HH.slot_ _cell 2 HCCell.component cell ] <> rightBorder <> bottomBorder
  where
    rightBorder = if beforeSudokuBorder toCol i 
      then [ borderCell ] else []
    bottomBorder = if customBottomSudokuBoxBorder i 
      then (\_ -> borderCell) <$> 1 .. (boardSize + internalBorders) else []

borderCell :: ∀ widget input. HH.HTML widget input
borderCell = HH.div [ HP.classes [HH.ClassName "ss-sudoku-border" ] ] []

internalBorders :: Int
internalBorders = boardRoot - 1

-- | Lets us put an empty row after the last cell in a right-most box
-- | For boardsize of 9, this is index 26, and 53, which are the last cells in the
-- | 2nd and 5th row
customBottomSudokuBoxBorder :: Int -> Boolean
customBottomSudokuBoxBorder index =
  beforeSudokuBorder toRow index &&
  pos % root == 0.0 && 
  pos / root == root
  where
    pos = inc $ toNumber $ toCol index
    root = toNumber boardRoot 