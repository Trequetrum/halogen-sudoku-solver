module App.Dirty where

import Prelude

import App.Button as Button
import App.HC.Board as HCBoard
import App.HC.Cell as HCCell
import App.HC.Option as HCOption
import Halogen as H
import Halogen.HTML as HH
import Sudoku.Common (Cell(..), Option(..))
import Test.Basic.Data (startingBoard)
import Type.Prelude (Proxy(..))


type Slots = 
  ( button :: ∀ query. H.Slot query Void Int 
  , option :: ∀ query. H.Slot query Option Int 
  , cell :: ∀ query. H.Slot query Cell Int 
  , board :: ∀ query. H.Slot query Cell Int 
  )

_button = Proxy :: Proxy "button"
_option = Proxy :: Proxy "option"
_cell = Proxy :: Proxy "cell"
_board = Proxy :: Proxy "board"

component :: ∀ query input output m. H.Component query input output m
component =
  H.mkComponent 
    { initialState: identity
    , render 
    , eval: H.mkEval H.defaultEval
    }

render :: ∀ state actions m. state -> H.ComponentHTML actions Slots m
render state =
  HH.div_ 
    [ HH.div_ [ HH.slot_ _button 0 Button.component  unit           ] 
    , HH.div_ [ HH.slot_ _option 1 HCOption.component (Option 1)    ]
    , HH.div_ [ HH.slot_ _cell   2 HCCell.component   (Cell 511)    ]
    , HH.div_ [ HH.slot_ _board  3 HCBoard.component  startingBoard ]
    ]