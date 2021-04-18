module App.HC.BoardRunner where

import Prelude

import App.HC.Cell (Output(..))
import App.HC.Cell as HCCell
import App.Parseboards (easyPuzzles, hardPuzzles, hardestPuzzles)
import Data.Array ((..))
import Data.Foldable (for_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Int as Ints
import Data.Map (toUnfoldable)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (floor, sqrt, (%))
import Stateful (Stateful(..), constructorString, unwrapStateful)
import Sudoku.Board (Board, modifyAtIndex, unconstrainedBoard, (!!))
import Sudoku.Cell (Cell, dropOptions, setOptions, toCell)
import Sudoku.Format (beforeSudokuBorder)
import Sudoku.Group (groupId, toColumn, toRow)
import Sudoku.Index (boardIndices)
import Sudoku.Index.Internal (Index)
import Sudoku.Option (indexOf, numOfOptions)
import Sudoku.Puzzle (Puzzle, MetaBoard, fromBoard)
import Sudoku.Strategy.Bruteforce (ladderTupleBruteForce)
import Sudoku.Strategy.Common (Strategy, advanceOrFinish, onlyAdvancing, stayOrFinish)
import Sudoku.Strategy.NTuples (enforceHiddenNTuples, enforceHiddenTuples, enforceNakedTuples)
import Sudoku.Strategy.NTuplesWithMeta (enforceNaked1Tuples, enforceNTuples)
import Type.Prelude (Proxy(..))
import Utility (inc)

type Slots = ( cell :: HCCell.Slot Index )

_cell = Proxy :: Proxy "cell"

type State = 
  { userPuzzle :: Stateful Puzzle
  , renderPuzzle :: Stateful Puzzle
  }

data Action
  = Blank
  | Reset
  | ConstrainAll
  | HandleCell Index HCCell.Output
  | NewPuzzle Puzzle
  | Solve
  | Enforce1Tuples
  | Enforce2Tuples
  | Enforce3Tuples
  | Enforce4Tuples

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
    untouchedPuzzle = Stable $ fromBoard unconstrainedBoard

setNewPuzzle :: Puzzle -> State -> State
setNewPuzzle puzzle state = state 
  { userPuzzle = Stable puzzle
  , renderPuzzle = Stable puzzle
  }

render :: ∀ m. State -> H.ComponentHTML Action Slots m
render state = HH.div
  [ HP.classes [ HH.ClassName "ss-solver-layout" ] ]
  [ HH.div [ HP.classes [HH.ClassName "ss-layout-heading" ] ]
    [ HH.h1_ [ HH.text "Sudoku Solver" ]
    , HH.h3_ [ HH.text "A Purescript Exercise Project" ]
    ]
  , HH.div [ HP.classes [HH.ClassName "ss-puzzle-container" ] ]
    [ makeHCBoard $ snd $ unwrapStateful $ state.renderPuzzle
    , makeHCMetaOutput state.renderPuzzle
    ]
  , hCActionsUi
  , selectAPuzzle
  ]

handleAction :: ∀ output m. Action → H.HalogenM State Action Slots output m Unit
handleAction Blank = H.modify_ \_ -> initialState
handleAction Reset = H.modify_ \st -> st { renderPuzzle = st.userPuzzle }
handleAction ConstrainAll = for_ boardIndices \i -> H.tell _cell i HCCell.Constrain
handleAction (NewPuzzle puzzle) = do 
  H.modify_ \st -> setNewPuzzle puzzle st
  handleAction ConstrainAll

handleAction (HandleCell index output) = case output of
  (ToggleOn option) -> H.modify_ $ updateStateCell setOptions (toCell option) index
  (ToggleOff option) -> H.modify_ $ updateStateCell dropOptions (toCell option) index
  (SetTo cell) -> H.modify_ $ updateStateCell const cell index

handleAction Solve = do
  handleStrategy ladderTupleBruteForce
  handleAction ConstrainAll

handleAction Enforce1Tuples = handleStrategy $ enforceNTuples 1
handleAction Enforce2Tuples = handleStrategy $ enforceNTuples 2
handleAction Enforce3Tuples = handleStrategy $ enforceNTuples 3
handleAction Enforce4Tuples = handleStrategy $ enforceNTuples 4

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
-- Action Buttons
------------------------------------------------------------------------

hCActionsUi :: ∀ widget. HH.HTML widget Action
hCActionsUi = HH.div
  [ HP.classes [ HH.ClassName "ss-actions-container" ] ]
  [ HH.button
    [ HE.onClick \_ -> Blank ]
    [ HH.text "Blank Board" ]
  , HH.button
    [ HE.onClick \_ -> Reset ]
    [ HH.text "Reset" ]
  , HH.button
    [ HE.onClick \_ -> ConstrainAll ]
    [ HH.text "Enlarge Singletons" ]
  , HH.button
    [ HE.onClick \_ -> Solve ]
    [ HH.text "Solve" ]
  , HH.button
    [ HE.onClick \_ -> Enforce1Tuples ]
    [ HH.text "1 Tuples" ]
  , HH.button
    [ HE.onClick \_ -> Enforce2Tuples ]
    [ HH.text "2 Tuples" ]
  , HH.button
    [ HE.onClick \_ -> Enforce3Tuples ]
    [ HH.text "3 Tuples" ]
  , HH.button
    [ HE.onClick \_ -> Enforce4Tuples ]
    [ HH.text "4 Tuples" ]
  ]

------------------------------------------------------------------------
-- Pre-build Puzzle Buttons
------------------------------------------------------------------------

selectAPuzzle :: ∀ widget. HH.HTML widget Action
selectAPuzzle = HH.div 
  [ HP.classes [ HH.ClassName "ss-select-sudoku" ] ] 
  [ HH.h5_ [ HH.text "Select An Easy Sudoku" ]
  , HH.div_(mapWithIndex selectPuzzleButton easyPuzzles)
  , HH.h5_ [ HH.text "Select A Hard Sudoku" ]
  , HH.div_(mapWithIndex selectPuzzleButton hardPuzzles)
  , HH.h5_ [ HH.text "Select A Hardest Sudoku" ]
  , HH.div_ (mapWithIndex selectPuzzleButton hardestPuzzles)
  ]

-- | 
-- | <button class="mdc-button mdc-button--outlined mdc-button--raised">
-- |   <span class="mdc-button__label">Outlined Button</span>
-- | </button>
-- | 
selectPuzzleButton :: ∀ widget. Int -> Puzzle -> HH.HTML widget Action
selectPuzzleButton i x = HH.button
  [ HE.onClick \_ -> NewPuzzle x
  , HP.classes 
    [ HH.ClassName "mdc-button"
    , HH.ClassName "mdc-button--outlined"
    , HH.ClassName "mdc-button--raised"
    ]
  ]
  [ HH.span 
    [ HP.classes [ HH.ClassName "mdc-button__label" ] ]
    [ HH.text $ show $ i + 1 ]
  ]

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
  $ boardIndices >>= makeLayoutCells
  where
    makeLayoutCells :: Index -> Array (H.ComponentHTML Action Slots m)
    makeLayoutCells i = let 

      rightBorder = if beforeSudokuBorder (toColumn >>> groupId >>> indexOf)  i 
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
      beforeSudokuBorder (toRow >>> groupId >>> indexOf) index &&
      pos % root == 0.0 && 
      pos / root == root
      where
        pos = inc $ toNumber $ indexOf $ groupId $ toColumn index
        root = floor $ sqrt $ toNumber numOfOptions 

------------------------------------------------------------------------
-- Metaboard info readout
------------------------------------------------------------------------

makeHCMetaOutput :: ∀ widget input. Stateful Puzzle -> HH.HTML widget input
makeHCMetaOutput puzzle = HH.div 
  [ HP.classes [ HH.ClassName "ss-metaboard-info-readout" ] ]
  [ HH.h4_ [ HH.text "Meta-information From Strategies" ] 
  , HH.hr_
  , HH.span_ $
    [ HH.text "Puzzle State: " 
    , HH.text $ constructorString puzzle
    ] <> tagAddon
  , tupleList
  ]
  where
    meta :: MetaBoard
    meta = fst $ unwrapStateful puzzle

    tagAddon :: Array (HH.HTML widget input)
    tagAddon = case puzzle of 
      (Invalid err _) -> [ HH.br_, HH.text $ show err ]
      otherwise -> []
    
    tupleList :: HH.HTML widget input
    tupleList = HH.ul_ $ 
      (\(Tuple count num) -> HH.li_ 
        [ HH.text $ show count <> ": " <> show num ]
      ) <$> toUnfoldable meta.tupleCount

