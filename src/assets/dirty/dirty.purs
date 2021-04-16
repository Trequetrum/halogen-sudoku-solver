module Asset.Dity where

import Prelude

import Control.Monad.State (State, get, modify_)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))

addIncState :: Number -> State Int Number
addIncState n = do
  state <- get
  modify_ (_ + 1)
  pure $ toNumber state + n

addIncTuple :: Tuple Int Number -> Tuple Int Number
addIncTuple (Tuple state val) = Tuple (state + 1) (toNumber state + val)