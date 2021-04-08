module App.HC.Option where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sudoku.Common (Option)
import Sudoku.Format (optionString)
import Web.UIEvent.MouseEvent (MouseEvent, ctrlKey)

-- | Token is set by the parent component
-- | Showing is internal state that toggles whether the token is visible
type State =  
  { option :: Option
  , showing :: Boolean
  }

-- | Bounce: Do nothing in response to some event
-- | Toggle: Change Token visibility in response to some event
-- | Set: Emit state's option to parent component
data Action = Bounce | Toggle | Set Option

-- | Copy input into state
initialState :: Option -> State
initialState option =
  { option
  , showing : false
  }

-- | This component can output Options
component :: ∀ query m. H.Component query Option Option m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

-- | How to respond to a mouse event from the DOM
onClick :: State -> MouseEvent -> Action
onClick state evt = case (ctrlKey evt), state.showing of
  true, true -> Set state.option
  true, false -> Bounce
  false, _ -> Toggle 

-- | Renders at Option as a string character in a div
render :: ∀ slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "ss-option" ] 
    , HE.onClick $ onClick state
    ]
    [ HH.text $ if state.showing then optionString state.option else "" ]

handleAction :: ∀ slots m. Action → H.HalogenM State Action slots Option m Unit
handleAction Bounce       = do pure unit
handleAction Toggle       = H.modify_ \st -> st { showing = not st.showing }
handleAction (Set option) = H.raise option