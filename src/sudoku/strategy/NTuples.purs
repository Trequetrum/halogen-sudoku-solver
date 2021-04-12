module Sudoku.Strategy.NTuples where

import Prelude

import Sudoku.Board (Action, Board, Index, allIndicesGroup, batchDropOptions, filterIndices, indexedCells, indicesExPeers, repChange)
import Control.MonadZero (guard)
import Data.Array (concat, foldl, length, (\\))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Cell (Cell, allCells, allOptionsCell, cellsOfSize, countOptions, dropOptions, isSuperset, notDisjoint, toggleCell)
import Sudoku.Cell as Cells
import Sudoku.Puzzle (Puzzle)
import Sudoku.Strategy.Common (Strategy, ladderStrats)
import Utility (filterUnless)

type NTuple = Tuple Cell (Array Index)

-- | NTuples typically exist only in the context of a group.
-- | * tupleRel :: (Cell -> Cell -> Boolean) - if tupleRel (possible cell) (cell in board) == true, then 
-- |    (cell in board) is a counted cell in the board
-- | * illegalRel :: (Int -> Int -> Boolean) - if 
-- |    illegalRel (countOptions possible cell) (counted cells in board) == true, 
-- |    then we short circuit and return Nothing. Indicating we have a board that isn't legal
-- |
-- | * Int :: the size of the tuple we're searching for
-- | * Board :: The cells we check for options in
-- | * Array Index :: The indices of the board to search through
findNTuplesByPred :: (Cell -> Cell -> Boolean) -> (Int -> Int -> Boolean) ->
  Int -> Board -> Array Index -> Maybe (Array NTuple)
findNTuplesByPred tupleRel illegalRel size board group =
  filterUnless isTupleGroup illegalGroup tuples
  where
    isTupleGroup (Tuple cell indices) = countOptions cell == length indices
    illegalGroup (Tuple cell indices) = illegalRel (countOptions cell) (length indices)

    tuples :: Array NTuple
    tuples = do
      comb <- cellsOfSize size
      guard $ isSuperset allOptionsCell comb
      let matches = filterIndices (tupleRel comb) board group
      guard $ length matches > 0
      pure $ Tuple comb matches

findNakedNTuples :: Int -> Board -> Array Index -> Maybe (Array NTuple)
findNakedNTuples = findNTuplesByPred isSuperset (<)

findHiddenNTuples :: Int -> Board -> Array Index -> Maybe (Array NTuple)
findHiddenNTuples = findNTuplesByPred notDisjoint (>)

-- A way to find tuples within a group/subgroup of a given board. Tuples here are defined via the number
-- of options in a possible cell and the number of cells in the group that a predicate returns
-- true for. If those two values are equal, then the possible cell and the indices of cells (in the board)
-- are returned
--
-- If you're planning to find all tuples of a certain kind, this is an optimized way to accomplish that.
-- This is because options and indices for matches can be ignored when iterating through the 
-- list of all possible cells (Previous matches narrow the search-space for future matches.)
--
-- tupleRel :: (Cell -> Cell -> Boolean) - if tupleRel (possible cell) (board cell) == true, then 
--    (board cell) is counted
-- illegalRel :: (Int -> Int -> Boolean) - if 
--    illegalRel (countOptions possible cell) (count board cell) == true, then we s short circuit
--    and return Nothing. Indicating we have a board that isn't legal
findTuplesByPred :: (Cell -> Cell -> Boolean) -> (Int -> Int -> Boolean) ->
  Board -> Array Index -> Maybe (Array NTuple)
findTuplesByPred tupleRel illegalRel board group = 
  find allOptionsCell allCells group (Just [])
  where
    find :: Cell -> List Cell -> Array Index ->  Maybe (Array NTuple) -> Maybe (Array NTuple)
    find _ _ _ Nothing = Nothing
    find _ Nil _ jt = jt
    find options (currComb : restCombs) subgroup jt@(Just tuples)
      | not (Cells.isValid options) = jt
      | countOptions currComb > length subgroup = jt
      | not (options `isSuperset` currComb) = find options restCombs subgroup jt
      | otherwise = let
        
        matches :: Array Index
        matches = filterIndices (tupleRel currComb) board subgroup

      in if countOptions currComb == length matches 
        then find 
          (dropOptions currComb options)
          restCombs
          (subgroup \\ matches)
          (Just $ tuples <> (pure $ Tuple currComb matches))
        else if illegalRel (countOptions currComb) (length matches)
        then Nothing
        else find options restCombs subgroup jt

findNakedTuples :: Board -> Array Index -> Maybe (Array NTuple)
findNakedTuples = findTuplesByPred isSuperset (<)

findHiddenTuples :: Board -> Array Index -> Maybe (Array NTuple)
findHiddenTuples = findTuplesByPred notDisjoint (>)


-- A very basic strategy, if a cell has only one option, then remove that option
-- from its peers. This has its own implemention because Naked 1 Tuples exist outside 
-- the context of sudoku groups and effect all groups they're a member of.
-- This means there are optimisation/shortcuts unique enforceNaked1Tuples which makes
-- this the fastest strategy to run.
enforceNaked1Tuples :: Strategy
enforceNaked1Tuples puzzle = 
  if length actions < 1 then Stable puzzle
  else Advancing $ batchDropOptions actions <$> puzzle
  where
    board :: Board
    board = snd puzzle

    actions :: Array Action
    actions = do
      Tuple i cell <- indexedCells board
      guard $ countOptions cell == 1
      iPeer <- indicesExPeers i
      let action = Tuple iPeer cell
      guard $ repChange board action
      pure action


-- If a group has N cells with the same N options, then no other cells
-- in that group may contain any of those N options.
--
-- The simplest case of this are singleton cells. If, for example, a group
-- has a cell with exclusively a 2, then the option of a 2 can be removed 
-- from all other cells in the group.
--
-- If a group has N + 1 Cells with the same N options, then it's not a 
-- valid board. Return a finished board
enforceNakedTuples' :: (Board -> Array Index -> Maybe (Array NTuple)) -> Strategy
enforceNakedTuples' findTuples puzzle = case maybeActions of
  Nothing -> Finished puzzle
  Just actions 
    | length actions < 1 -> Stable puzzle
    | otherwise -> Advancing $ batchDropOptions actions <$> puzzle
  where
    board :: Board
    board = snd puzzle

    maybeActions :: Maybe (Array Action)
    maybeActions = concat <$> sequence do 
      group <- allIndicesGroup 
      pure do
        tuples <- findTuples board group
        pure do
          Tuple comb indices <- tuples
          i <- group \\ indices
          let action = Tuple i comb
          guard $ repChange board action
          pure action

enforceNakedTuples :: Strategy
enforceNakedTuples = enforceNakedTuples' findNakedTuples

enforceNakedNTuples :: Int -> Strategy
enforceNakedNTuples 1 = enforceNaked1Tuples
enforceNakedNTuples n = enforceNakedTuples' (findNakedNTuples n)

-- If a group only has N possible cells for a combination of options of
-- length N, then those N cells may only contain that combination.
--
-- The simplest case of this are combinations of length 1. If, for example,
-- a group only has 1 possible cell that may contain a 2, then that cell 
-- can contain only a 2 and all other options in that cell may be removed.
--
-- If a group has less than N possible cells for a combination of options of
-- length N, then it's not a valid board. Return a finished board
enforceHiddenTuples' :: (Board -> Array Index -> Maybe (Array NTuple)) -> Strategy
enforceHiddenTuples' findTuples puzzle = case maybeActions of
  Nothing -> Finished puzzle
  Just actions 
    | length actions < 1 -> Stable puzzle
    | otherwise -> Advancing $ batchDropOptions actions <$> puzzle
  where
    board :: Board
    board = snd puzzle

    maybeActions :: Maybe (Array Action)
    maybeActions = concat <$> sequence do
      group <- allIndicesGroup
      pure do
        tuples <- findTuples board group
        pure do
          Tuple comb indices <- tuples
          i <- indices
          let action = Tuple i $ toggleCell comb
          guard $ repChange board action
          pure action

enforceHiddenTuples :: Strategy
enforceHiddenTuples = enforceHiddenTuples' findHiddenTuples 

enforceHiddenNTuples :: Int -> Strategy
enforceHiddenNTuples = findHiddenNTuples >>> enforceHiddenTuples'

ladderTuples :: Strategy
ladderTuples = ladderStrats $ NonEmptyArray
  [ enforceNakedNTuples 1  -- Very effective early on and runs very fast
  , enforceHiddenNTuples 1 -- Runs fast, anything done here creates Naked 1 Tuples
  , enforceNakedNTuples 2  -- Not sure this helps actually
  , rollingEnforceHiddenTuples    -- Optimized, but still pretty heavy, runs last
  ]

rollingEnforceHiddenTuples :: Strategy
rollingEnforceHiddenTuples inputPuzzle = foldl 
  (\brd group -> enforce group brd)
  (Stable inputPuzzle)
  allIndicesGroup
  where
    enforce :: Array Index -> Stateful Puzzle -> Stateful Puzzle
    enforce group fp@(Finished _) = fp
    enforce group sPuzzle = let
     
      puzzle :: Puzzle
      puzzle = unwrapStateful sPuzzle

      board :: Board
      board = snd puzzle

      maybeActions :: Maybe (Array Action)
      maybeActions = do
        tuples <- findHiddenTuples board group
        pure do
          Tuple comb indices <- tuples
          i <- indices
          let action = Tuple i $ toggleCell comb
          guard $ repChange board action
          pure action
      
    in case maybeActions of
      Nothing -> Finished puzzle
      Just actions 
        | length actions < 1 -> sPuzzle
        | otherwise -> Advancing $ batchDropOptions actions <$> puzzle