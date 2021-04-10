module App.HC.BoardRunner where

import Prelude

import App.HC.Cell (Output(..))
import App.HC.Cell as HCCell
import Data.Array (concat, modifyAt, (!!), (..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, maybe)
import Data.Tuple (Tuple(..), snd)
import Debug (spy)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math ((%))
import Safe.Coerce (coerce)
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Common (Board, Cell(..), Index, Puzzle, StatefulStrategy, Strategy, boardIndices, boardRoot, boardSize, dropOptions, setOptions, toCell, toCol, toRow)
import Sudoku.Format (beforeSudokuBorder)
import Sudoku.Strategy.Bruteforce (ladderTupleBruteForce)
import Sudoku.Strategy.NTuples (enforceHiddenNTuples, enforceHiddenTuples, enforceNakedNTuples, enforceNakedTuples)
import Test.Basic.Data (startingPuzzle)
import Type.Prelude (Proxy(..))
import Utility (inc)

type Slots = ( cell :: HCCell.Slot Int )

_cell = Proxy :: Proxy "cell"

type SudokuBoard = Stateful Puzzle

type State = 
  { userBoard :: SudokuBoard
  , renderBoard :: SudokuBoard
  }

data Action
  = Reset
  | ConstrainAll
  | HandleCell Int HCCell.Output
  | NewBoard SudokuBoard
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
  { userBoard : untouchedBoard
  , renderBoard : untouchedBoard
  }
  where
    untouchedCells :: Board
    untouchedCells = coerce $ const 511 <$> boardIndices

    untouchedBoard :: SudokuBoard
    untouchedBoard = Advancing $ Tuple ({ metaData: {}, metaBoard: [] }) untouchedCells

setNewBoard :: SudokuBoard -> State -> State
setNewBoard board state = state 
  { userBoard = board
  , renderBoard = board
  }

render :: ∀ m. State -> H.ComponentHTML Action Slots m
render state = HH.div
  [ HP.classes [ HH.ClassName "ss-solver-layout" ] ]
  [ HH.h1_
    [ HH.text "Halogen6 Sudoku" ]
  , HH.div
    [ HP.classes [ HH.ClassName "ss-actions-container" ] ]
    [ HH.button
      [ HE.onClick \_ -> Reset ]
      [ HH.text "Reset" ]
    , HH.button
      [ HE.onClick \_ -> NewBoard $ Advancing startingPuzzle]
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
  , makeHCBoard $ snd $ unwrapStateful $ state.renderBoard
  ]

handleAction :: ∀ output m. Action → H.HalogenM State Action Slots output m Unit
handleAction Reset = H.modify_ \_ -> initialState
handleAction ConstrainAll = for_ boardIndices \i -> H.tell _cell i HCCell.Constrain
handleAction (NewBoard board) = do 
  H.modify_ \st -> setNewBoard board st
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

updateStateCell :: (Cell -> Cell -> Cell) -> Cell -> Int -> State -> State
updateStateCell update with index state = state 
  { userBoard = updateFn state.userBoard
  , renderBoard = updateFn state.renderBoard
  }
  where
    updateFn :: SudokuBoard -> SudokuBoard
    updateFn puzzle = map (\b -> fromMaybe b $ modifyAt index (update with) b) <$> puzzle
  

handleStrategy :: ∀ output m. Strategy -> H.HalogenM State Action Slots output m Unit
handleStrategy strat = H.modify_ \st -> st { renderBoard = forceStrategy strat st.renderBoard }

forceStrategy :: Strategy -> StatefulStrategy
forceStrategy strat = unwrapStateful >>> strat

------------------------------------------------------------------------
-- Building the board
------------------------------------------------------------------------

borderCell :: ∀ widget input. HH.HTML widget input
borderCell = HH.div [ HP.classes [HH.ClassName "ss-sudoku-border" ] ] []

makeHCCell :: ∀ m. Board -> Index -> H.ComponentHTML Action Slots m
makeHCCell board i = maybe borderCell (\c -> HH.slot _cell i HCCell.component c $ HandleCell i) (board !! i)

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
        then (\_ -> borderCell) <$> 1 .. (boardSize + internalBorders) 
        else []

      in [ makeHCCell board i ] 
        <> rightBorder 
        <> bottomBorder

    internalBorders :: Int
    internalBorders = boardRoot - 1

    -- | Lets us put an empty row after the last cell in a right-most box
    -- | For boardsize of 9, this is index 26, and 53, which are the last 
    -- | cells in the 2nd and 5th row
    customBottomSudokuBoxBorder :: Int -> Boolean
    customBottomSudokuBoxBorder index =
      beforeSudokuBorder toRow index &&
      pos % root == 0.0 && 
      pos / root == root
      where
        pos = inc $ toNumber $ toCol index
        root = toNumber boardRoot 