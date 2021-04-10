module App.HC.Cell where

import Prelude

import Data.Maybe (Maybe(..))
import Debug (spy)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sudoku.Common (Cell, Option, allOptions, countOptions, firstOption, hasOption, toCell, toggleOptions)
import Sudoku.Format (optionString)
import Web.UIEvent.MouseEvent (MouseEvent, ctrlKey)

type Slot id = H.Slot Query Output id

data Query a
  = Constrain a

type Input = Cell

data Output 
  = ToggleOn Option 
  | ToggleOff Option 
  | SetTo Cell

data State 
  = Unconstrained Cell 
  | Constrained Option

data Action 
  = Bounce
  | Unconstrain 
  | Toggle Option 
  | Force Option
  | Receive Input

component :: ∀ m. 
  H.Component Query Input Output m
component =
  H.mkComponent 
    { initialState: updateState
    , render 
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction 
      , receive = receive
      , handleQuery = handleQuery
      }
    }

updateState :: Input -> State
updateState cell 
  | countOptions cell == 1 = Constrained $ firstOption cell
  | otherwise = Unconstrained cell

render :: ∀ slots m. 
  State -> H.ComponentHTML Action slots m
render (Unconstrained cell) = HH.div
  [ HP.classes [ HH.ClassName "ss-cell-container" ] ]
  $ makeOption cell <$> allOptions
render (Constrained option) = HH.div_
  [ HH.div
    [ HP.classes [ HH.ClassName "ss-forced-cell" ] 
    , HE.onClick clickConstrained
    ]
    [ HH.text $ optionString option ]
  ]
  
makeOption :: ∀ slots m. 
  Cell -> Option -> H.ComponentHTML Action slots m
makeOption cell option = HH.div
  [ HP.classes [ HH.ClassName optionClass ] 
  , HE.onClick $ clickOption option
  ]
  [ HH.text char ]
  where
    open = hasOption option cell
    optionClass = if open 
      then "ss-option-open" 
      else "ss-option-closed"
    char = if open 
      then optionString option 
      else ""

clickConstrained :: MouseEvent -> Action
clickConstrained evt = if ctrlKey evt
  then Unconstrain
  else Bounce

clickOption :: Option -> MouseEvent -> Action
clickOption option evt = if ctrlKey evt
  then Force option
  else Toggle option

-- We write a function to handle queries when they arise.
handleQuery :: forall a slots m. Query a -> H.HalogenM State Action slots Output m (Maybe a)
handleQuery (Constrain a) = do
  state <- H.get
  case state of
    (Unconstrained stateCell) -> H.put $ updateState stateCell
    _ -> handleAction Bounce
  pure (Just a)

handleAction :: ∀ slots m. Action → H.HalogenM State Action slots Output m Unit
handleAction Bounce = do
  pure unit

handleAction Unconstrain = do
  state <- H.get
  case state of
    (Constrained stateOption) ->
      H.put $ Unconstrained $ toCell stateOption
    _ -> handleAction Bounce

handleAction (Toggle option) = do
  state <- H.get
  case state of
    (Unconstrained stateCell) -> do
      let update = toggleOptions (toCell option) stateCell
      let output = if hasOption option stateCell
        then ToggleOff option
        else ToggleOn option
      H.put $ updateState update
      H.raise output
    _ -> handleAction Bounce

handleAction (Force option) = do
  state <- H.get
  case state of
    (Unconstrained stateCell) -> 
      if hasOption option stateCell 
      then do
        H.put $ Constrained option
        H.raise $ SetTo $ toCell option
      else if countOptions stateCell == 1
      then do
        H.put $ Constrained $ firstOption stateCell
      else handleAction Bounce
    _ -> handleAction Bounce

handleAction (Receive newCell) = do
  state <- H.get
  case state of
    (Constrained stateOption) ->
      if countOptions newCell == 1 && firstOption newCell == stateOption
      then handleAction Bounce
      else H.put $ Unconstrained newCell
    (Unconstrained stateCell) -> 
      if stateCell == newCell
      then handleAction Bounce
      else H.put $ Unconstrained newCell

receive :: Input -> Maybe Action
receive = Just <<< Receive