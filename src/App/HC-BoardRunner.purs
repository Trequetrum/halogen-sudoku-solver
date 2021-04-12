module App.HC.BoardRunner where

import Prelude

import App.HC.Cell (Output(..))
import App.HC.Cell as HCCell
import App.Parseboards (easyPuzzles, hardPuzzles, hardestPuzzles)
import Data.Array (concat, mapWithIndex, (..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Int as Ints
import Data.Tuple (Tuple(..), snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (floor, sqrt, (%))
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Board, Index, boardIndices, modifyAtIndex, toCol, toRow, unconstrainedBoard, (!!))
import Sudoku.Cell (Cell, dropOptions, setOptions, toCell)
import Sudoku.Format (beforeSudokuBorder)
import Sudoku.Option (numOfOptions)
import Sudoku.Puzzle (Puzzle)
import Sudoku.Strategy.Bruteforce (ladderTupleBruteForce)
import Sudoku.Strategy.Common (Strategy, advanceOrFinish, onlyAdvancing, stayOrFinish)
import Sudoku.Strategy.NTuples (enforceHiddenNTuples, enforceHiddenTuples, enforceNakedNTuples, enforceNakedTuples)
import Test.Basic.Data (startingPuzzle)
import Type.Prelude (Proxy(..))
import Utility (inc)

type Slots = ( cell :: HCCell.Slot Index )

_cell = Proxy :: Proxy "cell"

type State = 
  { userPuzzle :: Stateful Puzzle
  , renderPuzzle :: Stateful Puzzle
  }

data Action
  = Reset
  | ConstrainAll
  | HandleCell Index HCCell.Output
  | NewPuzzle Puzzle
  | Solve
  | Naked1Tuples
  | Hidden1Tuples
  | NakedTuples
  | HiddenTuples

component :: ∀ query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: State
initialState = 
  { userPuzzle : untouchedPuzzle
  , renderPuzzle : untouchedPuzzle
  }
  where
    untouchedPuzzle :: Stateful Puzzle
    untouchedPuzzle = Stable $ Tuple ({ metaData: {}, metaBoard: [] }) unconstrainedBoard

setNewPuzzle :: Puzzle -> State -> State
setNewPuzzle puzzle state = state 
  { userPuzzle = Stable puzzle
  , renderPuzzle = Stable puzzle
  }

render :: ∀ m. State -> H.ComponentHTML Action Slots m
render state = HH.div
  [ HP.classes [ HH.ClassName "ss-solver-layout" ] ]
  [ HH.h1_ [ HH.text "Halogen6 Sudoku" ]
  , HH.div
    [ HP.classes [ HH.ClassName "ss-actions-container" ] ]
    [ HH.button
      [ HE.onClick \_ -> Reset ]
      [ HH.text "Reset" ]
    , HH.button
      [ HE.onClick \_ -> NewPuzzle startingPuzzle]
      [ HH.text "Starting Board" ]
    , HH.button
      [ HE.onClick \_ -> Solve ]
      [ HH.text "Solve" ]
    , HH.button
      [ HE.onClick \_ -> Naked1Tuples ]
      [ HH.text "Naked 1 Tuples" ]
    , HH.button
      [ HE.onClick \_ -> Hidden1Tuples ]
      [ HH.text "Hidden 1 Tuples" ]
    , HH.button
      [ HE.onClick \_ -> NakedTuples ]
      [ HH.text "All Naked Tuples" ]
    , HH.button
      [ HE.onClick \_ -> HiddenTuples ]
      [ HH.text "All Hidden Tuples" ]
    , HH.button
      [ HE.onClick \_ -> ConstrainAll ]
      [ HH.text "Ctrl+Click Cells" ]
    ]
  , makeHCBoard $ snd $ unwrapStateful $ state.renderPuzzle
  , HH.div 
    [ HP.classes [ HH.ClassName "ss-select-sudoku" ] ] 
    [ HH.h5_ [ HH.text "Select An Easy Sudoku" ]
    , HH.div_(mapWithIndex selectPuzzleButton easyPuzzles)
    , HH.h5_ [ HH.text "Select A Hard Sudoku" ]
    , HH.div_(mapWithIndex selectPuzzleButton hardPuzzles)
    , HH.h5_ [ HH.text "Select A Hardest Sudoku" ]
    , HH.div_ (mapWithIndex selectPuzzleButton hardestPuzzles)
    ]
  ]

selectPuzzleButton :: ∀ widget. Int -> Puzzle -> HH.HTML widget Action
selectPuzzleButton i x = HH.button
  [ HE.onClick \_ -> NewPuzzle x ]
  [ HH.text $ show $ i + 1 ]

handleAction :: ∀ output m. Action → H.HalogenM State Action Slots output m Unit
handleAction Reset = H.modify_ \_ -> initialState
handleAction ConstrainAll = for_ boardIndices \i -> H.tell _cell i HCCell.Constrain
handleAction (NewPuzzle puzzle) = do 
  H.modify_ \st -> setNewPuzzle puzzle st
  handleAction ConstrainAll

handleAction (HandleCell index output) = case output of
  (ToggleOn option) -> H.modify_ $ updateStateCell setOptions (toCell option) index
  (ToggleOff option) -> H.modify_ $ updateStateCell dropOptions (toCell option) index
  (SetTo cell) -> H.modify_ $ updateStateCell const cell index

handleAction Solve = handleStrategy ladderTupleBruteForce
handleAction Naked1Tuples = handleStrategy $ enforceNakedNTuples 1
handleAction Hidden1Tuples = handleStrategy$ enforceHiddenNTuples 1
handleAction NakedTuples = handleStrategy enforceNakedTuples 
handleAction HiddenTuples = handleStrategy enforceHiddenTuples

------------------------------------------------------------------------
-- Action Helpers
------------------------------------------------------------------------

updateStateCell :: (Cell -> Cell -> Cell) -> Cell -> Index -> State -> State
updateStateCell update with index state = state 
  { userPuzzle = updateFn state.userPuzzle
  , renderPuzzle = updateFn state.renderPuzzle
  }
  where
    updateFn :: Stateful Puzzle -> Stateful Puzzle
    updateFn puzzle = map (modifyAtIndex (update with) index) <$> puzzle

handleStrategy :: ∀ output m. Strategy -> H.HalogenM State Action Slots output m Unit
handleStrategy strat = H.modify_ \st -> st { renderPuzzle = stayOrFinish $ onlyAdvancing strat $ advanceOrFinish st.renderPuzzle }

------------------------------------------------------------------------
-- Building the board
------------------------------------------------------------------------

borderCell :: ∀ widget input. HH.HTML widget input
borderCell = HH.div [ HP.classes [HH.ClassName "ss-sudoku-border" ] ] []

makeHCCell :: ∀ m. Board -> Index -> H.ComponentHTML Action Slots m
makeHCCell board i = HH.slot _cell i HCCell.component (board !! i) $ HandleCell i

makeHCBoard :: ∀ m. Board -> H.ComponentHTML Action Slots m
makeHCBoard board = HH.div
  [ HP.classes [ HH.ClassName "ss-board-container" ] ]
  $ concat $ makeLayoutCells <$> boardIndices
  where
    makeLayoutCells :: Index -> Array (H.ComponentHTML Action Slots m)
    makeLayoutCells i = let 

      rightBorder = if beforeSudokuBorder toCol i 
        then [ borderCell ] 
        else []
      bottomBorder = if customBottomSudokuBoxBorder i 
        then (\_ -> borderCell) <$> 1 .. (numOfOptions + internalBorders) 
        else []

      in [ makeHCCell board i ] 
        <> rightBorder 
        <> bottomBorder

    internalBorders :: Int
    internalBorders = (Ints.floor $ sqrt $ toNumber numOfOptions) - 1

    -- | Lets us put an empty row after the last cell in a right-most box
    -- | For boardsize of 9, this is index 26, and 53, which are the last 
    -- | cells in the 2nd and 5th row
    customBottomSudokuBoxBorder :: Index -> Boolean
    customBottomSudokuBoxBorder index =
      beforeSudokuBorder toRow index &&
      pos % root == 0.0 && 
      pos / root == root
      where
        pos = inc $ toNumber $ toCol index
        root = floor $ sqrt $ toNumber numOfOptions 