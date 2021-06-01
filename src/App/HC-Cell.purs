-- |
-- | This halogen component is a little box that displays either a set
-- | of up to 9 options or a singular enlarged option
-- |
module App.HC.Cell where

import Prelude 
import Sudoku.OSet (OSet, countOptions, firstOption, hasOption, toOSet, toggleOptions, trustFirstOption)
import Sudoku.Option (Option, allOptions, asString, invalidOption)

import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent, ctrlKey)

-- | This is most of the H.Slot type of this component. 
-- | Parent components can/should use use this type and supply the ID type
-- | they're using
-- |
-- | If this type chacnges in the future, that creates a single source. 
-- | And the compilor will lets any parent components know about mismatches
-- |
type Slot id = H.Slot Query Output id

-- | An unconstrained set with cardinality 1 is semantically the same as a
-- | constrained option, but they display differently. This lets a parent
-- | component transform an OSet with size 1 into an Option in this Cell's 
-- | state
data Query a
  = Constrain a

-- | The OSet that this component displays. If it has cardinality 1, it is 
-- | automatically contrained to an Option and displayed in a larger font
type Input = OSet

-- | Parent components can register to know any changes to the state of this 
-- | Cell. 
data Output 
  = ToggleOn Option 
  | ToggleOff Option 
  | SetTo OSet

-- | The state is a set of options and a single option. Single options are 
-- | given extra promenance so they're easy to spot. A set with cardinality 1
-- | is not an illegal state
data State 
  = Unconstrained OSet 
  | Constrained Option

-- | Actions that Cells support:
-- |  * Bounce - No nothing, used internally to ignore events under 
-- |      certain circumstances
-- |  * Unconstrain - Transform an Option in a Set with just that Option
-- |  * Toggle - add/remove an option from a set
-- |  * Force - Transform a Set into an Option
-- |  * Receive - Set the OSet that this component displays (See Input)
data Action 
  = Bounce
  | Unconstrain 
  | Toggle Option 
  | Force Option
  | Receive Input

-- | Create the top level component, wires together the various event 
-- | listeners including how this component is rendered.
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

-- | Convert input into component state
updateState :: Input -> State
updateState cell 
  | countOptions cell == 1 = Constrained $ fromMaybe invalidOption $ firstOption cell
  | otherwise = Unconstrained cell

-- | Render either a tic-tac-toe/phone style 9-grid of options or a single 
-- | larger option depending on the current state of this component
-- |
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
    [ HH.text $ asString option ]
  ]

-- | How to render each option in an OSet
makeOption :: ∀ slots m. 
  OSet -> Option -> H.ComponentHTML Action slots m
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
      then asString option 
      else ""

-- | Event listener when an option is clicked. This event is ignored 
-- | unless it's a ctrl-click
clickConstrained :: MouseEvent -> Action
clickConstrained evt = if ctrlKey evt
  then Unconstrain
  else Bounce

-- | Event listener when an option within a set is clicked. Behaviour 
-- | depends on whether it's a ctrl-click
clickOption :: Option -> MouseEvent -> Action
clickOption option evt = if ctrlKey evt
  then Force option
  else Toggle option

-- | Handle queries from the parent component. (See the Query type)
handleQuery :: ∀ a slots m. Query a -> H.HalogenM State Action slots Output m (Maybe a)
handleQuery (Constrain a) = do
  state <- H.get
  case state of
    (Unconstrained stateCell) -> H.put $ updateState stateCell
    _ -> handleAction Bounce
  pure (Just a)

receive :: Input -> Maybe Action
receive = Just <<< Receive

-- | Logic for how each Action is handled (See the Action ADT)
handleAction :: ∀ slots m. Action → H.HalogenM State Action slots Output m Unit
handleAction = case _ of

  Bounce -> pure unit

  Unconstrain -> do
    state <- H.get
    case state of
      (Constrained stateOption) ->
        H.put $ Unconstrained $ toOSet stateOption
      _ -> handleAction Bounce

  (Toggle option) -> do
    state <- H.get
    case state of
      (Unconstrained stateCell) -> do
        let update = toggleOptions (toOSet option) stateCell
        let output = if hasOption option stateCell
          then ToggleOff option
          else ToggleOn option
        H.put $ updateState update
        H.raise output
      _ -> handleAction Bounce

  (Force option) -> do
    state <- H.get
    case state of
      (Unconstrained stateCell) -> 
        if hasOption option stateCell 
        then do
          H.put $ Constrained option
          H.raise $ SetTo $ toOSet option
        else if countOptions stateCell == 1
        then do
          H.put $ Constrained $ fromMaybe invalidOption $ firstOption stateCell
        else handleAction Bounce
      _ -> handleAction Bounce

  (Receive newCell) -> 
    if countOptions newCell == 1
    then H.put $ Constrained (trustFirstOption newCell)
    else do
      state <- H.get
      case state of
        (Constrained _) ->
          H.put $ Unconstrained newCell
        (Unconstrained stateCell) -> 
          if stateCell == newCell
          then handleAction Bounce
          else H.put $ Unconstrained newCell
