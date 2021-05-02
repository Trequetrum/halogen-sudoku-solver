module App.HC.BoardRunner where

import Prelude

import App.HC.Cell (Output(..))
import App.HC.Cell as HCCell
import App.Parseboards (easyPuzzles, hardPuzzles, hardestPuzzles)
import Data.Array (length, (..))
import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Foldable (for_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Int as Ints
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (now)
import Error (message, name)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (floor, sqrt, (%))
import Stateful (Stateful(..), constructorString, unwrapStateful)
import Sudoku.Board (Board, modifyAtIndex, unconstrainedBoard, (!!))
import Sudoku.Format (beforeSudokuBorder, boardToForcedString, boardToIntString, boardToString)
import Sudoku.Group (groupId, toColumn, toRow)
import Sudoku.Index (boardIndices)
import Sudoku.Index.Internal (Index)
import Sudoku.OSet (OSet, dropOptions, setOptions, toOSet)
import Sudoku.Option (indexOf, numOfOptions)
import Sudoku.Puzzle (MetaBoard, Puzzle, fromBoard)
import Sudoku.Strategy.Bruteforce (affLadderTupleBruteForce, ladderTupleBruteForce)
import Sudoku.Strategy.Common (stayOrFinish)
import Sudoku.Strategy.MetaNTuples (ladderTuples, rollingEnforceNTuples)
import Type.Prelude (Proxy(..))
import Utility (affFn, inc)

type Slots = ( cell :: HCCell.Slot Index )

_cell = Proxy :: Proxy "cell"

type State = 
  { computing :: Boolean
  , userPuzzle :: Stateful Puzzle
  , renderPuzzle :: Stateful Puzzle
  , stratTime :: Maybe Milliseconds
  }

data Action
  = Blank
  | Reset
  | ConstrainAll
  | HandleCell Index HCCell.Output
  | NewPuzzle Puzzle
  | Solve
  | AsyncSolve
  | Enforce1Tuples
  | Enforce2Tuples
  | Enforce3Tuples
  | Enforce4Tuples
  | LadderTuples

component :: ∀ query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: State
initialState = 
  { computing : false
  , userPuzzle : untouchedPuzzle
  , renderPuzzle : untouchedPuzzle
  , stratTime : Nothing
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
  [ HH.div 
    [ HP.id "computingOverlay"
    , HP.style if state.computing then "display: block" else "display: none"
    ] []
  , HH.div [ HP.classes [HH.ClassName "ss-layout-heading" ] ]
    [ HH.h1_ [ HH.text "Sudoku Solver" ]
    , HH.h3_ [ HH.text "A Purescript Exercise Project" ]
    ]
  , HH.div [ HP.classes [HH.ClassName "ss-puzzle-container" ] ]
    [ makeHCBoard $ snd $ unwrapStateful $ state.renderPuzzle
    , makeHCMetaOutput state
    ]
  , hCActionsUi
  , selectAPuzzle
  , otherInfo state
  ]

handleAction :: ∀ output m. MonadAff m => Action → 
  H.HalogenM State Action Slots output m Unit
handleAction Blank = H.modify_ \_ -> initialState
handleAction Reset = H.modify_ \st -> st { renderPuzzle = st.userPuzzle }
handleAction ConstrainAll = for_ boardIndices \i -> H.tell _cell i HCCell.Constrain
handleAction (NewPuzzle puzzle) = do 
  H.modify_ \st -> setNewPuzzle puzzle st
  handleAction ConstrainAll

handleAction (HandleCell index output) = case output of
  (ToggleOn option) -> H.modify_ $ updateStateCell setOptions (toOSet option) index
  (ToggleOff option) -> H.modify_ $ updateStateCell dropOptions (toOSet option) index
  (SetTo cell) -> H.modify_ $ updateStateCell const cell index

handleAction Solve = do
  handleStrategy (affFn ladderTupleBruteForce)
  handleAction ConstrainAll

handleAction AsyncSolve = do
  handleStrategy affLadderTupleBruteForce
  handleAction ConstrainAll

handleAction Enforce1Tuples = handleStrategy (affFn $ rollingEnforceNTuples 1)
handleAction Enforce2Tuples = handleStrategy (affFn $ rollingEnforceNTuples 2)
handleAction Enforce3Tuples = handleStrategy (affFn $ rollingEnforceNTuples 3)
handleAction Enforce4Tuples = handleStrategy (affFn $ rollingEnforceNTuples 4)
handleAction LadderTuples = handleStrategy (affFn ladderTuples)

------------------------------------------------------------------------
-- Action Helpers
------------------------------------------------------------------------

updateStateCell :: (OSet -> OSet -> OSet) -> OSet -> Index -> State -> State
updateStateCell update with index state = state 
  { userPuzzle = updateFn state.userPuzzle
  , renderPuzzle = updateFn state.renderPuzzle
  }
  where
    updateFn :: Stateful Puzzle -> Stateful Puzzle
    updateFn puzzle = map (modifyAtIndex (update with) index) <$> puzzle

handleStrategy :: ∀ output m.  
  MonadAff m => (Puzzle -> Aff (Stateful Puzzle)) -> 
  H.HalogenM State Action Slots output m Unit
handleStrategy strat = do
  H.modify_ _{ computing = true }
  H.liftAff $ delay $ Milliseconds 50.0
  state <- H.get
  startTime <- H.liftEffect now
  strated <- H.liftAff $ strat $ unwrapStateful state.renderPuzzle
  endTime <- H.liftEffect now
  H.put state
    { computing = false
    , renderPuzzle = stayOrFinish strated
    , stratTime = Just $ diff (toDateTime endTime) (toDateTime startTime) 
    }
  

------------------------------------------------------------------------
-- Action Buttons
------------------------------------------------------------------------

hCActionsUi :: ∀ widget. HH.HTML widget Action
hCActionsUi = HH.div
  [ HP.classes [ HH.ClassName "ss-actions-container" ] ]
  [ HH.h5_ [ HH.text "Sudoku Actions" ]
  , makeActionButton Blank "Blank Board"
  , makeActionButton Reset "Reset"
  , makeActionButton ConstrainAll "Enlarge Singletons"
  , HH.h5_ [ HH.text "Sudoku Algorithms" ]
  , makeActionButton Solve "Solve"
  , makeActionButton AsyncSolve "Async Solve"
  , makeActionButton Enforce1Tuples "1 Tuples"
  , makeActionButton Enforce2Tuples "2 Tuples"
  , makeActionButton Enforce3Tuples "3 Tuples"
  , makeActionButton Enforce4Tuples "4 Tuples"
  , makeActionButton LadderTuples "Ladder Tuples"
  ]

makeActionButton :: ∀ widget. Action -> String -> HH.HTML widget Action
makeActionButton action strng = HH.button
  [ HE.onClick \_ -> action 
  , HP.classes 
    [ HH.ClassName "mdc-button"
    , HH.ClassName "mdc-button--outlined"
    , HH.ClassName "mdc-button--raised"
    ] 
  ]
  [ HH.text strng ]

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
-- Place to shove other info and stuff
------------------------------------------------------------------------

otherInfo :: ∀ widget. State -> HH.HTML widget Action
otherInfo state = HH.div 
  [ HP.classes [ HH.ClassName "ss-infos-container" ] ]
  [ HH.div_ 
    [ HH.h5_ [ HH.text "Solved Options for each Board Cell" ]
    , HH.p_ [ HH.text $ boardToForcedString $ snd $ unwrapStateful state.renderPuzzle ]
    , HH.h5_ [ HH.text "Integer Encoding of the Board State" ]
    , HH.p_ [ HH.text $ boardToIntString $ snd $ unwrapStateful state.renderPuzzle ]
    ]
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

makeHCMetaOutput :: ∀ widget input. State -> HH.HTML widget input
makeHCMetaOutput state = HH.div 
  [ HP.classes [ HH.ClassName "ss-metaboard-info-readout" ] ] (
  [ HH.h4_ [ HH.text "Meta-information From Strategies" ] 
  , HH.hr_
  , HH.span_ $
    [ HH.text "Puzzle State: " 
    , HH.text $ constructorString state.renderPuzzle
    , HH.br_
    ] <> tagAddon
  , HH.hr_
  ] <> runTime <> tupleList <> bruteForce <> encodingInfo)
  where
    meta :: MetaBoard
    meta = fst $ unwrapStateful state.renderPuzzle

    tagAddon :: Array (HH.HTML widget input)
    tagAddon = case state.renderPuzzle of 
      (Invalid err _) -> [ HH.text (name err <> ":"), HH.br_, HH.text (message err) ]
      otherwise -> []
    
    runTime :: Array (HH.HTML widget input)
    runTime = case state.stratTime of
      (Just (Milliseconds ms)) -> [ HH.text ("Runtime: " <> show (Ints.floor ms) <> "ms"), HH.hr_ ]
      Nothing -> []

    tupleList :: Array (HH.HTML widget input)
    tupleList = if length lCount > 0 then
      [ HH.text "Tuple size: (naked,hidden,both,generated)" 
      , HH.ul_ $ 
        (\(Tuple count {naked, hidden, both, gen}) -> HH.li_ 
          [ HH.text $ show count <> ": (" <> 
            show naked <> "," <> show hidden <> "," <> show both <> "," <> show gen <> ")" 
          ]
        ) <$> lCount
      , HH.hr_
      ]
      else []
      where
        lCount = toUnfoldable meta.tupleCount

    bruteForce :: Array (HH.HTML widget input)
    bruteForce = if length guesses > 0 then
      [ HH.text "Brute Force Guesses:"
      , HH.ul_ guesses
      ]
      else []
      where 
        guesses = 
          (if meta.bruteForce.guessed > 0 then
            [ HH.li_ [ HH.text $ "Good: " <> show meta.bruteForce.guessed ] ]
            else []) <> 
          (if meta.bruteForce.backtrack > 0 then
            [ HH.li_ [ HH.text $ "Bad: " <> show meta.bruteForce.backtrack ] ]
            else [])

    encodingInfo :: Array (HH.HTML widget input)
    encodingInfo =
      [ HH.hr_ 
      , HH.text "Solved Options for each Board Cell" 
      , HH.p_ [ HH.text $ boardToForcedString $ snd $ unwrapStateful state.renderPuzzle ]
      , HH.text "Integer Encoding of the Board State"
      , HH.p_ [ HH.text $ boardToIntString $ snd $ unwrapStateful state.renderPuzzle ]
      ]