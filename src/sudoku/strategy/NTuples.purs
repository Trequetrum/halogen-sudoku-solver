-- | Check the Docs folder for an explanation of Naked and Hidden Tuples (And the 
-- | algorithm that finds and uses them). That is what is implemented here.
-- |
-- | The finding algorithsm have an N in their name if they search for a specific 
-- | size of tuple. They do not have an N if they search for tuples of any/all sizes
-- |
module Sudoku.Strategy.NTuples where

import Prelude

import Sudoku.Board (Action, Board, Index, allIndicesGroup, batchDropOptions, filterIndices, indexedCells, indicesExPeers, effective)
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
type FindingTupleAlgorithm = Board -> Array Index -> Maybe (Array NTuple)

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
  Int -> FindingTupleAlgorithm
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

-- | findNTuplesByPred specialized to find Naked N Tuples
-- |  * Int: is the size of tuples to look for
findNakedNTuples :: Int -> FindingTupleAlgorithm
findNakedNTuples = findNTuplesByPred isSuperset (<)

-- | findNTuplesByPred specialized to find Hidden N Tuples
-- |  * Int: is the size of tuples to look for
findHiddenNTuples :: Int -> FindingTupleAlgorithm
findHiddenNTuples = findNTuplesByPred notDisjoint (>)

-- | Much like findNTuplesByPred, but finds tuples of every size. This affords it a few optimisations.
-- |
-- | A way to find tuples within a group/subgroup of a given board. Tuples here are defined via the number
-- | of options in a possible cell and the number of cells in the group that a predicate returns
-- | true for. If those two values are equal, then the possible cell and the indices of cells (in the board)
-- | are returned
-- |
-- | If you're planning to find all tuples of a certain kind, this is an optimized way to accomplish that.
-- | This is because options and indices for matches can be ignored when iterating through the 
-- | list of all possible cells (Previous matches narrow the search-space for future matches.)
-- |
-- | tupleRel :: (Cell -> Cell -> Boolean) - if tupleRel (possible cell) (board cell) == true, then 
-- |    (board cell) is counted
-- | illegalRel :: (Int -> Int -> Boolean) - if 
-- |    illegalRel (countOptions possible cell) (count board cell) == true, then we s short circuit
-- |    and return Nothing. Indicating we have a board that isn't legal
findTuplesByPred :: (Cell -> Cell -> Boolean) -> (Int -> Int -> Boolean) -> FindingTupleAlgorithm
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

-- | findTuplesByPred specialized to find Naked Tuples
findNakedTuples :: FindingTupleAlgorithm
findNakedTuples = findTuplesByPred isSuperset (<)

-- | findTuplesByPred specialized to find Hidden Tuples
findHiddenTuples :: FindingTupleAlgorithm
findHiddenTuples = findTuplesByPred notDisjoint (>)


-- | A very basic strategy, if a cell has only one option, then remove that option
-- | from its peers. This has its own implemention because Naked 1 Tuples exist outside 
-- | the context of sudoku groups and effect all groups they're a member of.
-- | This means there are optimisation/shortcuts unique to enforceNaked1Tuples which makes
-- | this the fastest strategy to run.
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
      guard $ effective board action
      pure action


-- | Takes a tuple-finding algorithm and returns a strategy applies the returned
-- | tuples assuming though they are Naked Tuples
-- |
-- | If a group has N cells with the same N options, then no other cells
-- | in that group may contain any of those N options.
-- |
-- | The simplest case of this are singleton cells. If, for example, a group
-- | has a cell with exclusively a 2, then the option of a 2 can be removed 
-- | from all other cells in the group.
-- |
-- | If a group has N + 1 Cells with the same N options, then it's not a 
-- | valid board. Return a finished board
enforceNakedTuples' :: FindingTupleAlgorithm -> Strategy
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
          guard $ effective board action
          pure action

-- | enforceNakedTuples' specialized with a tuple-finding algorithm that finds
-- | tuples of any/all sizes
enforceNakedTuples :: Strategy
enforceNakedTuples = enforceNakedTuples' findNakedTuples

-- | enforceNakedTuples' specialized with a tuple-finding algorithm that finds
-- | tuples of the given size. Defers 1-Tuples to the specialised 
-- | enforceNaked1Tuples implementation
enforceNakedNTuples :: Int -> Strategy
enforceNakedNTuples 1 = enforceNaked1Tuples
enforceNakedNTuples n = enforceNakedTuples' (findNakedNTuples n)

-- | Takes a tuple-finding Algorithm and returns a strategy applies the returned
-- | tuples assuming though they are Hidden Tuples
-- |
-- | If a group only has N possible cells for a combination of options of
-- | length N, then those N cells may only contain that combination.
-- |
-- | The simplest case of this are combinations of length 1. If, for example,
-- | a group only has 1 possible cell that may contain a 2, then that cell 
-- | can contain only a 2 and all other options in that cell may be removed.
-- |
-- | If a group has less than N possible cells for a combination of options of
-- | length N, then it's not a valid board. Return a finished board
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
          guard $ effective board action
          pure action

-- | enforceHiddenTuples' specialized with a tuple-finding algorithm that finds
-- | tuples of any/all sizes
enforceHiddenTuples :: Strategy
enforceHiddenTuples = enforceHiddenTuples' findHiddenTuples 

-- | enforceHiddenTuples' specialized with a tuple-finding algorithm that finds
-- | tuples of the given size.
enforceHiddenNTuples :: Int -> Strategy
enforceHiddenNTuples = findHiddenNTuples >>> enforceHiddenTuples'

-- | Creates a ladder strategy out of the tuple strategies in this module
-- | See Sudoku.Strategy.Common for an explanation of ladderStrats
-- | Here is all how they ladder:
-- |   1. enforceNakedNTuples 1
-- |   2. enforceHiddenNTuples 1
-- |   3. enforceNakedNTuples 2
-- |   4. rollingEnforceHiddenTuples
ladderTuples :: Strategy
ladderTuples = ladderStrats $ NonEmptyArray
  [ enforceNakedNTuples 1  -- Very effective early on and runs very fast
  , enforceHiddenNTuples 1 -- Runs fast, anything done here creates Naked 1 Tuples
  , enforceNakedNTuples 2  -- Not sure this helps actually
  , rollingEnforceHiddenTuples    -- Optimized, but still pretty heavy, runs last
  ]

-- | The exact same algorithm as enforceHiddenTuples, but a slight difference 
-- | in implementation. rollingEnforceHiddenTuples will find all actions in a group,
-- | then apply those actions to the board before finding actions in the next group.
-- | 
-- | The hope is that later groups will benefit from the added information created
-- | from earlier groups. Running this strategy with The ST effect might bump performace
-- | a bit more as well. With a board size of 9, this makes at most 26 intermediate 
-- | copies of the board before returning the 27th copy. Sudoku boards are very small
-- | in memory, so the benefit is extremely small as well.
-- | 
-- | Still, it might be a good learning excersize
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
          guard $ effective board action
          pure action
      
    in case maybeActions of
      Nothing -> Finished puzzle
      Just actions 
        | length actions < 1 -> sPuzzle
        | otherwise -> Advancing $ batchDropOptions actions <$> puzzle