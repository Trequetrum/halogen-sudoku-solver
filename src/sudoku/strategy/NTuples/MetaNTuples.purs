-- | A re-implementation of the NTuples module, but using MetaData
-- | This implementation remembers the results of previous computations and attempts to not re-do
-- | the same work on future iterations. Likely to be pretty helpfull in combination with brute force
-- |
module Sudoku.Strategy.MetaNTuples where

import Prelude

import Control.MonadZero (guard)
import Data.Array (catMaybes, filter, length, nub, nubByEq, union, (..), (\\))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Map (Map, alter, lookup, singleton)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Error (Error(..))
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Board, batchDropOptions, filterIndices)
import Sudoku.Cell (Cell, allOptionsCell, asTokenString, cellsOfSize, countOptions, dropOptions, noOptionsCell, setOptions)
import Sudoku.Cell as Cll
import Sudoku.Group (Group, asIdString, groupIndices, groups)
import Sudoku.Index (Index)
import Sudoku.Puzzle (MetaBoard, Puzzle)
import Sudoku.Strategy.Common (Strategy, affLadderStrats, ladderStrats)
import Sudoku.Strategy.NTuple (NTuple, NTupleType(..), nTupleSize, peerExpand, sameTuple, toGroupActions)

updateTupleMeta :: Array NTuple -> MetaBoard -> MetaBoard
updateTupleMeta [] board = board
updateTupleMeta tuples board = board
  { tupleState = foldr 
      (\tuple -> alter (alterState tuple) tuple.group) 
      board.tupleState
      tuples
  , tupleCount = foldr 
      (\tuple -> alter (alterCount tuple) (nTupleSize tuple))
      board.tupleCount
      tuples
  }
  where
    alterCount 
      :: NTuple
      -> Maybe { naked :: Int, hidden :: Int, gen :: Int } 
      -> Maybe { naked :: Int, hidden :: Int, gen :: Int }
    alterCount tuple Nothing = alterCount tuple $ Just 
      { naked : 0
      , hidden : 0
      , gen : 0
      }
    alterCount tuple (Just n) = Just case tuple.tupleType of
      Gen ->    n { gen    = n.gen    + 1 }
      Naked ->  n { naked  = n.naked  + 1 }
      Hidden -> n { hidden = n.hidden + 1 }

    alterState :: NTuple -> Maybe (Map Int (Tuple Cell (Array Index))) -> Maybe (Map Int (Tuple Cell (Array Index)))
    alterState tuple Nothing = Just $ singleton (nTupleSize tuple) (Tuple tuple.options tuple.position)
    alterState tuple (Just sizeMap) = Just $ alter (alterSizeMap tuple) (nTupleSize tuple) sizeMap

    alterSizeMap :: NTuple -> Maybe (Tuple Cell (Array Index)) -> Maybe (Tuple Cell (Array Index))
    alterSizeMap tuple Nothing = Just $ Tuple tuple.options tuple.position
    alterSizeMap tuple (Just (Tuple optns posns)) = Just $ Tuple (setOptions optns tuple.options) (union posns tuple.position)


tupleState :: Int -> Group -> MetaBoard -> Tuple Cell (Array Index)
tupleState size group metaboard = 
  foldl foldSizes (Tuple noOptionsCell []) $ taken <$> 1 .. size
  where
    taken :: Int -> Maybe (Tuple Cell (Array Index))
    taken size' = lookup group metaboard.tupleState >>= lookup size'

    foldSizes
      :: (Tuple Cell (Array Index)) 
      -> Maybe (Tuple Cell (Array Index)) 
      -> (Tuple Cell (Array Index))
    foldSizes acc Nothing = acc
    foldSizes (Tuple cell indices) (Just (Tuple nxtCell nxtIndices)) =
      Tuple (setOptions cell nxtCell) (nub $ indices <> nxtIndices)

isTupleState :: MetaBoard -> Group -> NTuple -> Boolean
isTupleState metaboard group tuple@{options} = isJust do
  stateGroup <- lookup group metaboard.tupleState
  stateSpace <- lookup (countOptions options) stateGroup
  guard $ isSuperset 
    { tupleType: Gen
    , group
    , options: fst stateSpace
    , position: snd stateSpace 
    } tuple
  pure true

isSuperset :: NTuple -> NTuple -> Boolean
isSuperset 
  {group: lGroup, options: lOptions, position: lPosition} 
  {group: rGroup, options: rOptions, position: rPosition} 
  = (lGroup == rGroup) && oDiff && iDiff
  where
    oDiff = Cll.isSuperset lOptions rOptions
    iDiff = length (rPosition \\ lPosition) == 0

-- | This is a metaSearch because it leans on reliable metadata to narrow the search-space. 
-- |
-- | On the other hand, since metadata here is stored and retrived between searches, succesive iterations
-- | don't have to re-compute any of the solved search state-space.
-- |
-- | This searches both naked and hidden tuples of the same size at the same time. They both narrow
-- | the search space in the same way, so this saves time on calling this function twice (once for each
-- | type of tuple) A consequence of this is that this function returns actions instead of tuples the way
-- | other search algorithms do. It's just easier to consolidate two lists than to return Hidden and Naked
-- | tuples separatly.
-- |
-- | This accomplishes a lot in one fell swoop. The implementation could probably be tidied up a bit.
-- |
findNTuples :: Puzzle -> Int -> Group -> Either Error (Array NTuple)
findNTuples puzzle size group = nubByEq sameTuple <$> (append <$> nakedTuples <*> hiddenTuples)
  where
    board :: Board
    board = snd puzzle

    startState :: Tuple Cell (Array Index) 
    startState = tupleState size group $ fst puzzle

    options :: Cell
    options = dropOptions (fst startState) allOptionsCell

    indices :: Array Index
    indices = groupIndices group \\ snd startState

    combs :: Array Cell
    combs = filter (Cll.isSuperset options) $ cellsOfSize size

    nakedTuples :: Either Error (Array NTuple)
    nakedTuples = catMaybes <$> (sequence $ (checkFor Naked) <$> combs)

    hiddenTuples :: Either Error (Array NTuple)
    hiddenTuples = catMaybes <$> (sequence $ (checkFor Hidden) <$> combs)

    checkFor :: NTupleType -> Cell -> Either Error (Maybe NTuple)
    checkFor tupleType = case tupleType of
      Naked -> checkIf Naked Cll.isSuperset (<)
      Hidden -> checkIf Hidden Cll.notDisjoint (>)
      Gen -> checkIf Gen Cll.isSuperset (<)
    
    checkIf :: NTupleType -> 
      (Cell -> Cell -> Boolean) -> 
      (Int -> Int -> Boolean) -> 
      Cell -> Either Error (Maybe NTuple)
    checkIf tupleType tupleRel illegalRel comb = if failure
      then Left $ findingError tuple group
      else if success
      then Right (Just tuple)
      else Right Nothing
      where
        matches = filterIndices (tupleRel comb) board indices
        success = countOptions comb == length matches
        failure = countOptions comb `illegalRel` length matches
        tuple = 
          { tupleType: tupleType
          , group
          , options : comb 
          , position : matches
          }

findingError :: NTuple -> Group -> Error
findingError {tupleType, options} group = Error ("Impossible " <> 
  name <> " Tuple") ("Option(s) [" <> asTokenString options <> 
  "] are " <> relationShip <> show (countOptions options) <> " cell(s) in " <> asIdString group)
  where 
    name :: String
    name = case tupleType of
      Naked -> "Naked"
      Hidden -> "Hidden"
      Gen -> "General"

    relationShip :: String
    relationShip = case tupleType of
      Naked -> "a superset of "
      Hidden -> "not disjoint with "
      Gen -> "related to "

-- | This applies tuples found in a group immediately before searching the
-- | next group. This is more efficient, though a bit harder to reason about
-- | what "one iteration" of such a strategy is. 
rollingEnforceNTuples :: Int -> Strategy
rollingEnforceNTuples size inputPuzzle = foldl 
  folder (Stable inputPuzzle) groups
  where
    folder :: Stateful Puzzle -> Group -> Stateful Puzzle
    folder ip@(Invalid _ sPuzzle) _ = ip
    folder sop@(Solved _) _ = sop
    folder sPuzzle group = let

      puzzle :: Puzzle
      puzzle = unwrapStateful sPuzzle

      eTuples :: Either Error (Array NTuple)
      eTuples = findNTuples puzzle size group

    in case eTuples of
      (Left err) -> Invalid err puzzle
      (Right tuples)
        | length tuples < 1 -> sPuzzle
        | otherwise -> let
            expandedTuples = tuples >>= (peerExpand $ snd puzzle)
          in Advancing $ bimap 
          (updateTupleMeta expandedTuples) 
          (batchDropOptions $ expandedTuples
            >>= (toGroupActions $ snd puzzle) 
          )
          puzzle

ladderOrder :: NonEmptyArray Strategy
ladderOrder = NonEmptyArray
  [ rollingEnforceNTuples 1
  , rollingEnforceNTuples 2 
  , rollingEnforceNTuples 3 
  , rollingEnforceNTuples 4 
  ]

ladderTuples :: Strategy
ladderTuples = ladderStrats ladderOrder