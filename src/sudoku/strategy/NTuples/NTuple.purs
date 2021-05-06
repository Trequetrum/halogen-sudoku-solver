-- | Check the Docs folder for an explanation of Naked and Hidden Tuples (And the 
-- | algorithm that finds and uses them). That is what is implemented here.
-- |
module Sudoku.Strategy.NTuple where

import Prelude

import Control.MonadZero (guard)
import Data.Array (elem, length, (\\))
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Error (Error(..))
import Sudoku.Board (Action, Board, effective, filterIndices)
import Sudoku.Group (Group, asIdString, groupIndices, toGroupsIntersection)
import Sudoku.Index (Index)
import Sudoku.OSet (OSet, asTokenString, countOptions, isSuperset, notDisjoint, toggleOSet)

data NTupleType = Gen | Naked | Hidden | Both

derive instance eqNTupleType :: Eq NTupleType

type NTuple =
  { tupleType :: NTupleType
  , group :: Group
  , options :: OSet
  , position :: Array Index
  }

-- | This returns true if two tuples represent the same set of options
-- | at the same board position. It ignores how this tuples was found
-- | or generated (tupleType and group)
sameTuple :: NTuple -> NTuple -> Boolean
sameTuple 
  {options: lOptions, position: lPosition}
  {options: rOptions, position: rPosition} =
  (lOptions == rOptions) && 
  (lPosition == rPosition)

-- | Returns the size of an nTuple
nTupleSize :: NTuple -> Int
nTupleSize {options} = countOptions options

-- | Check if a specific set of options is a tuple within a group.
toTuple :: Board -> Group -> OSet -> Error \/ Maybe NTuple
toTuple board group = toTupleIn board group $ groupIndices group

-- | Check if a specific set of options is a tuple within some subgroup of a group.
toTupleIn :: Board -> Group -> Array Index -> OSet -> Error \/ Maybe NTuple
toTupleIn board group indices options = do
  nakedTuple <- makeTuple Naked 
    nakedMatches
    (compare (==) nakedMatches)
    (compare (<) nakedMatches)
  hiddenTuple <- makeTuple Hidden
    hiddenMatches
    (compare (==) hiddenMatches)
    (compare (>) hiddenMatches)
  pure case nakedTuple, hiddenTuple of 
    Nothing, t@(Just _)  -> t
    t@(Just _), Nothing  -> t
    (Just nT), (Just hT) -> Just nT{ tupleType = Both }
    Nothing, Nothing     -> Nothing
  where
    nakedMatches :: Array Index
    nakedMatches = filter isSuperset
    hiddenMatches :: Array Index
    hiddenMatches = filter notDisjoint

    compare rel matches = countOptions options `rel` length matches
    filter rel = filterIndices (rel options) board indices

    makeTuple :: NTupleType 
      -> Array Index 
      -> Boolean 
      -> Boolean 
      -> Error \/ Maybe NTuple
    makeTuple tupleType matches success fail = 
      case success, fail of
        _, true -> Left $ toError tuple
        true, _ -> Right (Just tuple)
        _, _    -> Right Nothing 
      where
        tuple = 
          { tupleType
          , group
          , options
          , position : matches
          }

-- | Blindly create an error based on an NTuple
toError :: NTuple -> Error
toError {tupleType, group, options, position} = Error ("Impossible " <> 
  name <> " Tuple") ("Option(s) [" <> asTokenString options <> 
  "] are " <> relationShip <> show (length position) <> " cell(s) in " <> asIdString group)
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
-- |
-- | This is faster than toActions, but does not generate actions for peers
-- | in related groups
-- |
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
  pure 
    if eGroup == group
    then tuple
    else 
      { tupleType : Gen
      , group : eGroup
      , options
      , position
      }