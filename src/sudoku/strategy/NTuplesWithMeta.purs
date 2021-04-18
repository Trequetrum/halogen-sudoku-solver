-- | A re-implementation of the NTuples module, but using MetaData
-- | This implementation remembers the results of previous computations and attempts to not re-do
-- | the same work on future iterations. Likely to be pretty helpfull in combination with brute force
-- |
module Sudoku.Strategy.NTuplesWithMeta where

import Prelude

import Control.MonadZero (guard)
import Data.Array (catMaybes, filter, head, length, nub, (..), (:), (\\))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (foldl, foldr)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map, alter, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Error (Error(..))
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Action, Board, batchDropOptions, effective, filterIndices, indexedCells)
import Sudoku.Cell (Cell, allOptionsCell, asTokenString, cellsOfSize, countOptions, dropOptions, noOptionsCell, setOptions)
import Sudoku.Cell as Cll
import Sudoku.Group (Group, asIdString, exPeerIndices, groupIndices, groups, toGroups)
import Sudoku.Index (Index)
import Sudoku.Puzzle (MetaBoard, Puzzle)
import Sudoku.Strategy.Common (Strategy, ladderStrats)
import Sudoku.Strategy.NTuples (NTuple, nTupleSize, toActions)

updateTupleMeta :: Group -> Puzzle -> Array NTuple -> Puzzle
updateTupleMeta group puzzle tuples = lmap updateMeta puzzle
  where
    updateMeta :: MetaBoard -> MetaBoard
    updateMeta mb = mb 
      { tupleState = alter alterGroup group mb.tupleState
      , tupleCount = foldr (alter alterCount) mb.tupleCount $ (nTupleSize <$> tuples)
      }

    alterCount :: Maybe Int -> Maybe Int
    alterCount Nothing = Just 1
    alterCount (Just n) = Just (n + 1) 

    alterGroup :: Maybe (Map Int (Tuple Cell (Array Index))) -> Maybe (Map Int (Tuple Cell (Array Index)))
    alterGroup Nothing = Just $ foldl tupleFolder (fromFoldable []) tuples
    alterGroup (Just sizeMap) = Just $ foldl tupleFolder sizeMap tuples

    tupleFolder :: Map Int (Tuple Cell (Array Index)) -> NTuple -> Map Int (Tuple Cell (Array Index))
    tupleFolder sizeMap nTuple@(Tuple cell indices) = alter alterIndices (countOptions cell) sizeMap
      where
        alterIndices :: Maybe (Tuple Cell (Array Index)) -> Maybe (Tuple Cell (Array Index))
        alterIndices Nothing = Just nTuple
        alterIndices (Just (Tuple inputCell inputIndices)) = Just $ 
          Tuple (setOptions inputCell cell) (nub $ indices <> inputIndices)

tupleState :: Int -> Group -> Puzzle -> Tuple Cell (Array Index)
tupleState size group puzzle = 
  foldl foldSizes (Tuple noOptionsCell []) $ taken <$> 1 .. size
  where
    --stateMap :: 
    stateMap :: Map Group (Map Int (Tuple Cell (Array Index)))
    stateMap = fst puzzle # _.tupleState

    taken :: Int -> Maybe (Tuple Cell (Array Index))
    taken size' = lookup group stateMap >>= lookup size'

    foldSizes :: (Tuple Cell (Array Index)) -> 
      Maybe (Tuple Cell (Array Index)) -> 
      (Tuple Cell (Array Index))
    foldSizes acc Nothing = acc
    foldSizes (Tuple cell indices) (Just (Tuple nxtCell nxtIndices)) =
      Tuple (setOptions cell nxtCell) (nub $ indices <> nxtIndices)

isTupleState :: Puzzle -> Group -> NTuple -> Boolean
isTupleState puzzle group tuple = isJust do
  stateGroup <- lookup group (fst puzzle # _.tupleState) 
  stateSpace <- lookup (countOptions $ fst tuple) stateGroup
  guard $ isSuperset stateSpace tuple
  pure true

isSuperset :: NTuple -> NTuple -> Boolean
isSuperset left right = oDiff && iDiff
  where
    oDiff = Cll.isSuperset (fst left) (fst right)
    iDiff = length (snd right \\ snd left) == 0

-- | This is a metaSearch because it leans on reliable metadata to narrow the search-space. 
-- |
-- | On the other hand, since metadata here is stored and retrived between searches, succesive iterations
-- | don't have to re-compute any of the solved search state-space.
-- |
-- | This searches both naked and hidden tuples of the same size at the same time. They both narrow
-- | the search space on the same way, so this saves time on calling this function twice (once for each
-- | type of tuple) A consequence of this is that this function returns actions instead of tuples the
-- | other search algorithms do. It's just easier to consolidate two lists than to return Hidden and Naked
-- | tuples separatly.
-- |
-- | This accomplishes a lot in one fell swoop. The implementation could probably be tidied up a bit.
-- |
findNTupleActions :: Int -> Puzzle -> Group -> Tuple Puzzle (Either Error (Array Action))
findNTupleActions size puzzle group = either 
  (Tuple puzzle <<< Left) 
  (\_ -> either 
    (Tuple puzzle <<< Left) 
    (\_ -> Tuple newMetaPuzzle $ Right $ nakedActions <> hiddenActions) 
    hiddenTuples
  )
  nakedTuples
  where
    board :: Board
    board = snd puzzle

    startState :: Tuple Cell (Array Index) 
    startState = tupleState size group puzzle

    options :: Cell
    options = dropOptions (fst startState) allOptionsCell

    indices :: Array Index
    indices = groupIndices group \\ snd startState

    combs :: Array Cell
    combs = filter (Cll.isSuperset options) $ cellsOfSize size

    nakedTuples :: Either Error (Array NTuple)
    nakedTuples = catMaybes <$> (sequence $ checkIfNaked <$> combs)

    hiddenTuples :: Either Error (Array NTuple)
    hiddenTuples = catMaybes <$> (sequence $ checkIfHidden <$> combs)

    newMetaPuzzle :: Puzzle
    newMetaPuzzle = updateTupleMeta group puzzle $ 
      (fromRight [] nakedTuples) <> (fromRight [] hiddenTuples)

    nakedActions :: Array Action
    nakedActions = case nakedTuples of
      (Left _) -> []
      (Right tuples) -> tuples >>= toActions true board group 
    
    hiddenActions :: Array Action
    hiddenActions = case nakedTuples of
      (Left _) -> []
      (Right tuples) -> tuples >>= toActions false board group 

    checkIfNaked :: Cell -> Either Error (Maybe NTuple)
    checkIfNaked comb = if failure 
      then Left $ Error "Impossible Naked Tuple" ("Options (" <> 
        asTokenString comb <> 
        ") are a superset of " <> 
        show (length matches) <> 
        "cells in " <> asIdString group)
      else if success
      then Right $ Just $ Tuple comb matches
      else Right Nothing
      where
        matches = filterIndices (Cll.isSuperset comb) board indices
        success = countOptions comb == length matches
        failure = countOptions comb < length matches

    checkIfHidden :: Cell -> Either Error (Maybe NTuple)
    checkIfHidden comb = if failure 
      then Left $ Error "Impossible Hidden Tuple" ("Option(s) (" <> 
        asTokenString comb <> 
        ") are not disjoint with " <> 
        show (length matches) <> 
        " cell(s) in " <> asIdString group)
      else if success
      then Right $ Just $ Tuple comb matches
      else Right Nothing
      where
        matches = filterIndices (Cll.notDisjoint comb) board indices
        success = countOptions comb == length matches
        failure = countOptions comb > length matches


-- | A very basic strategy, if a cell has only one option, then remove that option
-- | from its peers. This has its own implemention because Naked 1 Tuples exist outside 
-- | the context of sudoku groups and effect all groups they're a member of.
-- | This means there are optimisation/shortcuts unique to enforceNaked1Tuples which makes
-- | this the fastest strategy to run.
-- |
-- | The meta-version of this strategy must re-contextualize each tuple into it's groups to
-- | update the board state. This comes with some upfront cost that may be recouperated later.
enforceNaked1Tuples :: Strategy
enforceNaked1Tuples puzzle = 
  if length actions < 1 then Stable puzzle
  else Advancing $ batchDropOptions actions <$> newMetaPuzzle
  where
    board :: Board
    board = snd puzzle

    newMetaPuzzle :: Puzzle
    newMetaPuzzle = foldlWithIndex updateTupleMeta puzzle mappedGroups
    
    singletons :: Array NTuple
    singletons = do
      Tuple i cell <- indexedCells board
      guard $ countOptions cell == 1
      pure $ Tuple cell [i]

    mappedGroups :: Map Group (Array NTuple)
    mappedGroups = foldl singletonFolder (fromFoldable []) singletons

    singletonFolder :: Map Group (Array NTuple) -> NTuple -> Map Group (Array NTuple)
    singletonFolder groupMap tuple = foldl groupFolder groupMap groups
      where
        groups :: Array Group
        groups = fromMaybe [] $ toGroups <$> (head $ snd $ tuple)

        groupFolder :: Map Group (Array NTuple) -> Group -> Map Group (Array NTuple)
        groupFolder gMap group = alter alterTuple group gMap
          where
            alterTuple :: Maybe (Array NTuple) -> Maybe (Array NTuple)
            alterTuple Nothing = case isTupleState puzzle group tuple of
              false -> Just [tuple]
              true -> Nothing
            alterTuple jt@(Just tuples) = case isTupleState puzzle group tuple of
              false -> Just $ tuple : tuples
              true -> jt

    actions :: Array Action
    actions = do
      Tuple cell indices <- singletons
      i <- indices
      iPeer <- exPeerIndices i
      let action = Tuple iPeer cell
      guard $ effective board action
      pure action

enforceNTuples :: Int -> Strategy
enforceNTuples size inputPuzzle = foldl 
  folder (Stable inputPuzzle) groups
  where
    folder :: Stateful Puzzle -> Group -> Stateful Puzzle
    folder ip@(Invalid _ sPuzzle) _ = ip
    folder sop@(Solved _) _ = sop
    folder sPuzzle group = let

      (Tuple newMeta eActions) =
        findNTupleActions size (unwrapStateful sPuzzle) group

    in case eActions of
      (Left err) -> Invalid err newMeta
      Right actions 
        | length actions < 1 -> sPuzzle
        | otherwise -> Advancing $ batchDropOptions actions <$> newMeta

ladderTuples :: Strategy
ladderTuples = ladderStrats $ NonEmptyArray
  [ enforceNTuples 1
  , enforceNTuples 2 
  , enforceNTuples 3 
  , enforceNTuples 4 
  ]