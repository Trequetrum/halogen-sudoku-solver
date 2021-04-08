module App.Button where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State
  = { count :: Int }

data Action
  = Increment

component :: ∀ query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: ∀ slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.div_
    [ HH.p_
        [ HH.text $ "You clicked " <> show state.count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "Click me" ]
    ]

handleAction :: ∀ cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction Increment = H.modify_ \st -> st { count = st.count + 1 }
