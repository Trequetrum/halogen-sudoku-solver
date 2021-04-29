-- | Check the Docs folder for an explanation of Naked and Hidden Tuples (And the 
-- | algorithm that finds and uses them). That is what is implemented here.
-- |
module Sudoku.Strategy.NTuple where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concat, elem, foldl, length, (..), (\\))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Int (floor, toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Debug (spy)
import Error (Error(..))
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Action, Board, batchDropOptions, filterIndices, indexedCells, effective)
import Sudoku.Cell (Cell, allCells, allOptionsCell, cellsOfSize, countOptions, dropOptions, isSuperset, notDisjoint, toggleCell)
import Sudoku.Cell as Cells
import Sudoku.Group (Group, asIdString, exPeerIndices, groupIndices, groups, toGroupsIntersection)
import Sudoku.Index (Index)
import Sudoku.Option (numOfOptions)
import Sudoku.Puzzle (Puzzle)
import Sudoku.Strategy.Common (Strategy, ladderStrats)
import Unsafe.Coerce (unsafeCoerce)

data NTupleType = Gen | Naked | Hidden

derive instance eqNTupleType :: Eq NTupleType

type NTuple =
  { tupleType :: NTupleType
  , group :: Group
  , options :: Cell
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

-- | Turn a Tuple into effective actions ( Actions that will alter the
-- | state of the board that is passed in )
-- | If a tuple can be found in multiple groups, the actions returned 
-- | effect all groups
toActions :: Board -> NTuple -> Array Action
toActions board {options, position} = do
  group <- toGroupsIntersection position
  i <- groupIndices group
  let action = Tuple i if elem i position
    then toggleCell options
    else options
  guard $ effective board action
  pure action

-- | Turn a Tuple into effective actions ( Actions that will alter the
-- | state of the board that is passed in )
-- | Actions are constrained to a single group
toGroupActions :: Board -> NTuple -> Array Action
toGroupActions board {tupleType, group, options, position} = do
  i <- if tupleType == Hidden then position else groupIndices group \\ position
  let action = Tuple i $ if tupleType == Hidden then toggleCell options else options
  guard $ effective board action
  pure action

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