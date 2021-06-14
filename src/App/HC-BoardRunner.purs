-- | This is an opinionated UI wrapper for my Purescript Sudoku Solver
-- | This creates a sudoku board, displays meta-information, and allows
-- | various ways to select some puzzles to work with.
-- |
-- | Most of the high-level functions that the sudoku solver exposes are
-- | able to be evaluated through this Halogen-based web-UI.
-- |
-- | This component is fully self-contained (no input and no output)
module App.HC.BoardRunner where

import Prelude

import App.HC.Cell (Output(..))
import App.HC.Cell as HCCell
import App.Parseboards (easyPuzzles, hardPuzzles, hardestPuzzles)
import Data.Array (length, (..))
import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Int as Ints
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
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
import Sudoku.Format (beforeSudokuBorder, boardToForcedString, boardToIntString)
import Sudoku.Group (groupId, toColumn, toRow)
import Sudoku.Index (boardIndices)
import Sudoku.Index.Internal (Index)
import Sudoku.OSet (OSet, dropOptions, setOptions, toOSet)
import Sudoku.Option (indexOf, numOfOptions)
import Sudoku.Puzzle (MetaBoard, Puzzle, fromBoard)
import Sudoku.Strategy.Bruteforce (affLadderBruteForce, ladderAllStrats, ladderBruteForce, norvigBruteForce)
import Sudoku.Strategy.Common (stayOrFinish)
import Sudoku.Strategy.NTupleStrat (rollingEnforceNTuples)
import Sudoku.Strategy.Pointing (rollingEnforcePointing)
import Type.Prelude (Proxy(..))
import Utility (affFn, inc)

-- | The only other component we need is the individual Cells. They're Id by
-- | Index, and otherwise we let the Cell define it's own interface.
type Slots = ( cell :: HCCell.Slot Index )

-- | Type level string proxy for the slot value of cell
_cell :: Proxy "cell"
_cell = Proxy
-- _cell = Proxy :: Proxy "cell"

-- | Board Runner state nesseary for what is rendered.
-- | The sudoku solver doesn't track how long it takes to run any of its
-- | algorithms, so that's done separately here.
-- |
-- | * computing is a boolean that tells us whether the mouse should have 
-- |   a waiting symbol.
-- | * puzzleName is the title of the puzzle and depends on context (For 
-- |   example: which puzzle was selected, if any)
-- | * userPuzzle: All state the user had input into the puzzle
-- | * renderPuzzle: An amalgomation of user and algorithm inputs into the
-- |   puzzle
-- | * stratTime: How long did it take the last strategy to run?
-- |
type State = 
  { computing :: Boolean
  , puzzleName :: String
  , userPuzzle :: Stateful Puzzle
  , renderPuzzle :: Stateful Puzzle
  , stratTime :: Maybe Milliseconds
  }

-- | Most actions defer to some implementation in the sudoku solver. See
-- | that documentation for details.
-- | * Blank - Return to the board's initial state
-- | * Reset - Undo any state-changes realised with solving algorithms
-- | * HandleCell - Handle all state changes made by cells in the board
-- | * ... see sudoku solver implementation
-- |  
data Action
  = Blank
  | Reset
  | HandleCell Index HCCell.Output
  | NewPuzzle String Puzzle
  | Solve
  | AsyncSolve
  | NorvigSolve
  | LadderStrats
  | Enforce1Tuples
  | Enforce2Tuples
  | Enforce3Tuples
  | Enforce4Tuples
  | EnforcePointing

-- | Create the top level component, wires together the various event 
-- | listeners including how this component is rendered.
component :: ∀ query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

-- | The starting state of the board is a black board with all options for 
-- | every cell.
initialState :: State
initialState = 
  { computing : false
  , puzzleName : "BLANK BOARD"
  , userPuzzle : untouchedPuzzle
  , renderPuzzle : untouchedPuzzle
  , stratTime : Nothing
  }
  where
    untouchedPuzzle :: Stateful Puzzle
    untouchedPuzzle = Stable $ fromBoard unconstrainedBoard

-- | Logic for how to enter a new puzzle into component state
setNewPuzzle :: String -> Puzzle -> State -> State
setNewPuzzle label puzzle state = state 
  { puzzleName = label
  , userPuzzle = Stable puzzle
  , renderPuzzle = Stable puzzle
  }

-- | Currently descibes how this componat is rendered. 
-- |  * Sudoku board (81 cells in a grid)
-- |  * Sudoku board meta-information
-- |    * Some from the algorithms, some kept locally
-- |  * Pre-generated sudoku puzzles
-- |    * Easy
-- |    * hard
-- |    * hardest
-- |  * Buttons that run algorithms or otherwise alter the board state
-- |  * Some text describing how to use the UI.
-- |
-- | TODO: Update with some UX in mind to make this all a bit nicer to 
-- | interact with.
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
    [ HH.div [ HP.classes [HH.ClassName "ss-board-container" ] ]
      [ HH.h4_ [ HH.text state.puzzleName ]
      , makeHCBoard $ snd $ unwrapStateful $ state.renderPuzzle
      ]
    , makeHCMetaOutput state
    ]
  , hCActionsUi
  , selectAPuzzle
  , otherInfo
  ]

-- | Logic for how each Action is handled (See the Action ADT)
handleAction :: ∀ output m. MonadAff m => Action → 
  H.HalogenM State Action Slots output m Unit
handleAction = case _ of 
  Blank -> H.modify_ \_ -> initialState
  Reset -> H.modify_ \st -> st { renderPuzzle = st.userPuzzle }
  (NewPuzzle label puzzle) -> H.modify_ \st -> setNewPuzzle label puzzle st

  (HandleCell index output) -> case output of
    (ToggleOn option) -> H.modify_ $ updateStateCell setOptions (toOSet option) index
    (ToggleOff option) -> H.modify_ $ updateStateCell dropOptions (toOSet option) index
    (SetTo cell) -> H.modify_ $ updateStateCell const cell index

  Solve -> handleStrategy (affFn ladderBruteForce)
  AsyncSolve -> handleStrategy affLadderBruteForce
  NorvigSolve -> handleStrategy (affFn norvigBruteForce)
  LadderStrats -> handleStrategy (affFn ladderAllStrats)
  Enforce1Tuples -> handleStrategy (affFn $ rollingEnforceNTuples 1)
  Enforce2Tuples -> handleStrategy (affFn $ rollingEnforceNTuples 2)
  Enforce3Tuples -> handleStrategy (affFn $ rollingEnforceNTuples 3)
  Enforce4Tuples -> handleStrategy (affFn $ rollingEnforceNTuples 4)
  EnforcePointing -> handleStrategy (affFn $ rollingEnforcePointing)


------------------------------------------------------------------------
-- Action Helpers
------------------------------------------------------------------------

-- | Helper function describes how to react to events raised by the board Cells
-- | These are all user-events and as such update both the user and render puzzles.
-- | This updates the puzzle name to only if there currently isn't one.
updateStateCell :: (OSet -> OSet -> OSet) -> OSet -> Index -> State -> State
updateStateCell update with index state = state 
  { puzzleName = 
      if state.puzzleName == "BLANK BOARD"
      then "Custom Board"
      else state.puzzleName
  , userPuzzle = updateFn state.userPuzzle
  , renderPuzzle = updateFn state.renderPuzzle
  }
  where
    updateFn :: Stateful Puzzle -> Stateful Puzzle
    updateFn puzzle = map (modifyAtIndex (update with) index) <$> puzzle

-- | Helper function wraps any Sudoku Solver Strategy so it can be run and displayed in
-- | this component. Even though the Sudoku Solver is pure and doesn't require any effects,
-- | some strategies play around with running in the Aff monad (to ensure the website doesn't
-- | hang). 
-- | Currently this is dealt with unifomrly by lifting all strategies into Aff.
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

-- | How to render the buttons that update the sudoku
hCActionsUi :: ∀ widget. HH.HTML widget Action
hCActionsUi = HH.div
  [ HP.classes [ HH.ClassName "ss-actions-container" ] ]
  [ HH.h4_ [ HH.text "Sudoku Actions" ]
  , makeActionButton Blank "Blank Board"
  , makeActionButton Reset "Reset"
  , HH.h4_ [ HH.text "Sudoku Algorithms" ]
  , makeActionButton Solve "Solve"
  , makeActionButton AsyncSolve "Async Solve"
  , makeActionButton NorvigSolve "Norvig's Strat"
  , makeActionButton LadderStrats "Ladder Strat"
  , HH.div [] 
    [ HH.p_ [ HH.text "# Tuples:" ]
    , makeActionButton Enforce1Tuples "1"
    , makeActionButton Enforce2Tuples "2"
    , HH.br_
    , makeActionButton Enforce3Tuples "3"
    , makeActionButton Enforce4Tuples "4" 
    ]
  , makeActionButton EnforcePointing "Pointing"
  ]

-- | A single button wraps an MDC-Web button and registers an event
-- | listener for when it's clicked
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

-- | Buttons that let the user select pre-genereated puzzles. Along with
-- | hopefully self-explanitory headers for their relative difficulty 
selectAPuzzle :: ∀ widget. HH.HTML widget Action
selectAPuzzle = HH.div 
  [ HP.classes [ HH.ClassName "ss-select-sudoku" ] ] 
  [ HH.h4_ [ HH.text "Select An Easy Sudoku" ]
  , HH.div_(mapWithIndex (selectPuzzleButton "Easy Sudoku") easyPuzzles)
  , HH.h4_ [ HH.text "Select A Hard Sudoku" ]
  , HH.div_(mapWithIndex (selectPuzzleButton "Hard Sudoku") hardPuzzles)
  , HH.h4_ [ HH.text "Select A Hardest Sudoku" ]
  , HH.div_ (mapWithIndex (selectPuzzleButton "Hardest Sudoku") hardestPuzzles)
  ]

-- | A single button that selects a pre-generated puzzle
-- |
-- | <button class="mdc-button mdc-button--outlined mdc-button--raised">
-- |   <span class="mdc-button__label">Outlined Button</span>
-- | </button>
-- | 
selectPuzzleButton :: ∀ widget. String -> Int -> Puzzle -> HH.HTML widget Action
selectPuzzleButton boardPrefixName i x = HH.button
  [ HE.onClick \_ -> NewPuzzle (boardPrefixName <> " # " <> buttonLabel) x
  , HP.classes 
    [ HH.ClassName "mdc-button"
    , HH.ClassName "mdc-button--outlined"
    , HH.ClassName "mdc-button--raised"
    ]
  ]
  [ HH.span 
    [ HP.classes [ HH.ClassName "mdc-button__label" ] ]
    [ HH.text buttonLabel ]
  ]
  where
    buttonLabel :: String
    buttonLabel = show $ i + 1

------------------------------------------------------------------------
-- Place to shove other info and stuff
------------------------------------------------------------------------

actionButtonInfo :: Array (Tuple String String)
actionButtonInfo = 
  [ Tuple "Click Option" "Toggle whether an option is in the set contained by a given cell"
  , Tuple "Ctrl+Click Option" "Select an option for a cell or shrink an enlarged singleton"
  , Tuple "Blank Board" "Create a board where every cell has every option as an element" 
  , Tuple "Reset" "Undo any changes that any of the strategies have created (Remove their constraints)"
  , Tuple "Select Easy/Hard/Hardest #" "Create a board with one of the prefabricated Sudoku puzzles"
  ]

stratButtonInfo :: Array (Tuple String String)
stratButtonInfo = 
  [ Tuple "Solve" "If possible, let each cell have only 1 option in its set"
  , Tuple "Async Solve" "If solving requires a guess, put the guess in the JavaScript event queue"
  , Tuple "Norvig's Strat" "Strategy losely based on Peter Norvig's write-up (Link Below)"
  , Tuple "Ladder Strat" "Apply each strategy in turn, only advancing to the next if every previous strategy is stable"
  , Tuple "# Tuples" "For each group, enforce tuples of the given size using both naked and hidden search"
  ]

infoToLi :: ∀ widget action. Tuple String String -> HH.HTML widget action
infoToLi (Tuple emph txt) = HH.li_ [ HH.strong_ [ HH.text emph ], HH.text (": " <> txt) ]

relateLinksInfo :: Array {linkText :: String, href :: String, followup :: String}
relateLinksInfo =
  [ { linkText: "Github Repository"
    , href: "https://github.com/Trequetrum/halogen-sudoku-solver"
    , followup: "that builds this page" 
    }
  , { linkText: "# Tuples"
    , href: "https://github.com/Trequetrum/halogen-sudoku-solver/blob/main/NTupleAlgorithm.md"
    , followup: "algorithm and how it's implemented" 
    }
  , { linkText: "Peter Norvig's"
    , href: "http://norvig.com/sudoku.html"
    , followup: "write-up for solving Sudoku puzzles" 
    }
  ]

linksToLi :: ∀ widget action. {linkText :: String, href :: String, followup :: String} -> HH.HTML widget action
linksToLi { linkText, href, followup } = HH.li_ 
  [ HH.a [ HP.href href ] [ HH.text linkText ]
  , HH.text ( " " <> followup )
  ]

otherInfo :: ∀ widget action. HH.HTML widget action
otherInfo = HH.div 
  [ HP.classes [ HH.ClassName "ss-infos-container" ] ]
  [ HH.div_
    [ HH.h4_ [ HH.text "Sudoku Action Buttons" ]
    , HH.ul_ (infoToLi <$> actionButtonInfo)
    , HH.h4_ [ HH.text "Sudoku Algorithm Buttons" ]
    , HH.ul_ (infoToLi <$> stratButtonInfo)
    , HH.h4_ [ HH.text "Related Links" ]
    , HH.ul_ (linksToLi <$> relateLinksInfo)
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
  [ HP.classes [ HH.ClassName "ss-sudoku-board" ] ]
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
  [ HP.classes [ HH.ClassName "ss-metaboard-info-readout" ] ] 
  ( [ HH.h4_ [ HH.text "Meta-information From Strategies" ] 
    , HH.hr_
    , HH.span_ $
      [ HH.text "Puzzle State: " 
      , HH.text $ constructorString state.renderPuzzle
      , HH.br_
      ] <> tagAddon
    ] 
    <> runTime 
    <> tupleList 
    <> pointing 
    <> bruteForce 
    <> encodingInfo
  )
  where
    meta :: MetaBoard
    meta = fst $ unwrapStateful state.renderPuzzle

    tagAddon :: Array (HH.HTML widget input)
    tagAddon = case state.renderPuzzle of 
      (Invalid err _) -> [ HH.text (name err <> ":"), HH.br_, HH.text (message err) ]
      _ -> []
    
    runTime :: Array (HH.HTML widget input)
    runTime = case state.stratTime of
      (Just (Milliseconds ms)) -> [ HH.hr_, HH.text ("Runtime: " <> show (Ints.floor ms) <> "ms"), HH.br_ ]
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
      ]
      else []
      where
        lCount = toUnfoldable meta.tupleCount
    
    pointing :: Array (HH.HTML widget input)
    pointing = if meta.pointing > 0 then
      [ HH.text ("Pointing Count: " <> show meta.pointing) ]
      else []

    bruteForce :: Array (HH.HTML widget input)
    bruteForce =
      ( if meta.pointing > 0 then 
          [ HH.br_ ] 
        else []
      )
      <>
      ( if length guesses > 0 then
          [ HH.text "Brute Force Guesses:"
          , HH.ul_ guesses
          ]
        else []
      )
      where 
        guesses = 
          ( if meta.bruteForce.guessed > 0 then
              [ HH.li_ [ HH.text $ "Good: " <> show meta.bruteForce.guessed ] ]
            else []
          ) 
          <> 
          ( if meta.bruteForce.backtrack > 0 then
              [ HH.li_ [ HH.text $ "Bad: " <> show meta.bruteForce.backtrack ] ]
            else []
          )

    encodingInfo :: Array (HH.HTML widget input)
    encodingInfo =
      [ HH.hr_ 
      , HH.text "Solved Options for each Board Cell" 
      , HH.p_ [ HH.text $ boardToForcedString $ snd $ unwrapStateful state.renderPuzzle ]
      , HH.text "Integer Encoding of the Board State"
      , HH.p_ [ HH.text $ boardToIntString $ snd $ unwrapStateful state.renderPuzzle ]
      ]