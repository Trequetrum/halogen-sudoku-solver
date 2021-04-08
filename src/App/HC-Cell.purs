module App.HC.Cell where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sudoku.Common (Cell, Option, allOptions, allOptionsCell, countOptions, firstOption, hasOption, toCell, toggleOptions)
import Sudoku.Format (optionString)
import Web.UIEvent.MouseEvent (MouseEvent, ctrlKey)

data State = Unconstrained Cell | Constrained Option

data Action = Bounce 
  | Unconstrain 
  | Toggle Option 
  | Constrain Option

isConstrained :: State -> Boolean
isConstrained (Constrained _) = true
isConstrained (Unconstrained _) = false

component :: ∀ query m. 
  H.Component query Cell Cell m
component =
  H.mkComponent 
    { initialState
    , render 
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Cell -> State
initialState cell 
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
  then Constrain option
  else Toggle option

handleAction :: ∀ slots m. Action → H.HalogenM State Action slots Cell m Unit
handleAction Bounce = do 
  pure unit

handleAction Unconstrain = do
  H.put $ Unconstrained allOptionsCell
  H.raise allOptionsCell

handleAction (Toggle option) = do
  state <- H.get
  case state of
    (Unconstrained stateCell) -> do 
      let update = toggleOptions (toCell option) stateCell
      H.put $ initialState update
      H.raise update
    _ -> handleAction Bounce

handleAction (Constrain option) = do 
  state <- H.get
  case state of
    (Unconstrained stateCell) -> 
      if hasOption option stateCell 
      then do
        H.put $ Constrained option
        H.raise $ toCell option
      else handleAction Bounce
    _ -> handleAction Bounce
