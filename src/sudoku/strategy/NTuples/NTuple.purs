-- | Check the Docs folder for an explanation of Naked and Hidden Tuples (And the 
-- | algorithm that finds and uses them). That is what is implemented here.
-- |
module Sudoku.Strategy.NTuple where

import Prelude

import Control.MonadZero (guard)
import Data.Array (elem, (\\))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Error (Error(..))
import Sudoku.Board (Action, Board, effective)
import Sudoku.OSet (OSet, asTokenString, countOptions, toggleOSet)
import Sudoku.Group (Group, asIdString, groupIndices, toGroupsIntersection)
import Sudoku.Index (Index)
import Type.Prelude (Proxy(..))

data NTupleType = Gen | Naked | Hidden | Both

derive instance eqNTupleType :: Eq NTupleType

type NTuple =
  { tupleType :: NTupleType
  , group :: Group
  , options :: OSet
  , position :: Array Index
  }

sameTuple :: NTuple -> NTuple -> Boolean
sameTuple 
  {options: lOptions, position: lPosition}
  {options: rOptions, position: rPosition} =
  (lOptions == rOptions) && 
  (lPosition == rPosition)

-- | Returns the size of an nTuple
nTupleSize :: NTuple -> Int
nTupleSize {options} = countOptions options

toTuple :: Board -> Group -> OSet -> Variant (nothing :: Unit, error :: Error, tuple :: NTuple)
toTuple board group = toTupleIn board group $ groupIndices group

toTupleIn :: Board -> Group -> Array Index -> OSet -> Variant (nothing :: Unit, error :: Error, tuple :: NTuple)
toTupleIn board group indices options = inj (Proxy :: Proxy "nothing") unit

-- | Blindly create an error based on an NTuple
toError :: NTuple -> Error
toError {tupleType, group, options} = Error ("Impossible " <> 
  name <> " Tuple") ("Option(s) [" <> asTokenString options <> 
  "] are " <> relationShip <> show (countOptions options) <> " cell(s) in " <> asIdString group)
  where 
    name :: String
    name = case tupleType of
      Naked -> "Naked"
      Hidden -> "Hidden"
      Both -> "Naked & Hidden"
      Gen -> "General"

    relationShip :: String
    relationShip = case tupleType of
      Naked -> "a superset of "
      Hidden -> "not disjoint with "
      Both -> "a superset of & not disjoint with "
      Gen -> "related to "

-- | Turn a Tuple into effective actions ( Actions that will alter the
-- | state of the board that is passed in )
-- | If a tuple can be found in multiple groups, the actions returned 
-- | effect all groups
toActions :: Board -> NTuple -> Array Action
toActions board {options, position} = do
  group <- toGroupsIntersection position
  i <- groupIndices group
  let action = Tuple i if elem i position
    then toggleOSet options
    else options
  guard $ effective board action
  pure action

-- | Turn a Tuple into effective actions ( Actions that will alter the
-- | state of the board that is passed in )
-- | Actions are constrained to a single group
toGroupActions :: Board -> NTuple -> Array Action
toGroupActions board {tupleType, group, options, position} = case tupleType of 
  Naked -> actions nakedIndices identity
  Hidden -> actions position toggleOSet
  Both -> []
  Gen -> actions nakedIndices identity
  where
    actions :: Array Index -> (OSet -> OSet) -> Array Action
    actions indices toOptions = do
      i <- indices
      let action = Tuple i (toOptions options)
      guard $ effective board action
      pure action

    nakedIndices :: Array Index
    nakedIndices = groupIndices group \\ position

-- | Return an array of the given tuple along with any generated tuples
peerExpand :: Board -> NTuple -> Array NTuple
peerExpand board tuple@{tupleType, group, options, position} = do
  eGroup <- toGroupsIntersection position
  if eGroup == group
  then pure tuple
  else pure 
    { tupleType : Gen
    , group : eGroup
    , options
    , position
    }