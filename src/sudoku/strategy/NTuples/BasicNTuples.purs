-- | Check the Docs folder for an explanation of Naked and Hidden Tuples (And the 
-- | algorithm that finds and uses them). That is what is implemented here.
-- |
-- | The finding algorithms have an N in their name if they search for a specific 
-- | size of tuple. They do not have an N if they search for tuples of any/all sizes
-- |
module Sudoku.Strategy.BasicNTuples where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concat, foldl, length, (..), (\\))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Int (floor, toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Error (Error)
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Action, Board, batchDropOptions, effective, indexedCells)
import Sudoku.Group (Group, exPeerIndices, groupIndices, groups)
import Sudoku.Index (Index)
import Sudoku.OSet (OSet, allOptionsSet, countOptions, dropOptions, isSuperset, setsOfSize)
import Sudoku.OSet as OSets
import Sudoku.Option (numOfOptions)
import Sudoku.Puzzle (Puzzle)
import Sudoku.Strategy.Common (Strategy, ladderStrats)
import Sudoku.Strategy.NTuple (NTuple, toGroupActions, toTupleIn)

type FindingTupleAlgorithm = Board -> Group -> Error \/ Array NTuple

-- | Tuples typically exist only in the context of a group. 
-- | The list of cells given here encodes the different combinations of possible options this algorithm 
-- | searches through. It expects cells to be ordered by size (countOptions).
-- | 
-- | This ordering affords a few optimisations. Previous matches narrow the search-space for future matches.
-- | Options and indices for current/previous matches can be ignored when iterating through the rest of the
-- | possible cells
-- |
-- | A way to find tuples within a group/subgroup of a given board. Tuples here are defined via the number
-- | of options in a possible cell and the number of cells in the group that a predicate returns
-- | true for. If those two values are equal, then the possible cell and the indices of cells (in the board)
-- | are returned
findTuples :: List OSet -> Board -> Group -> Error \/ Array NTuple
findTuples optionCombinations board group = 
  find allOptionsSet optionCombinations (groupIndices group) (Right [])
  where
    find :: OSet -> List OSet -> Array Index ->  
      Error \/ Array NTuple -> Error \/ Array NTuple
    find _ _ _ err@(Left _) = err
    find _ Nil _ jt = jt
    find options (currComb : restCombs) subgroup jt@(Right tuples)
      | not (OSets.isValid options) = jt
      | countOptions currComb > length subgroup = jt
      | not (options `isSuperset` currComb) = find options restCombs subgroup jt
      | otherwise = case toTupleIn board group subgroup options of
        (Left e) -> Left e
        (Right (Nothing)) -> find options restCombs subgroup jt
        (Right (Just tuple)) -> find
          (dropOptions currComb options)
          restCombs
          (subgroup \\ tuple.position)
          (Right $ tuples <> pure tuple)


-- | findTuples specialized to find tuples of size n
-- |  * Int: is the size of tuples to look for
findNTuples :: Int -> FindingTupleAlgorithm
findNTuples size = findTuples (L.fromFoldable $ setsOfSize size)

-- | findTuples specialized to find tuples of every size
findAllTuples :: FindingTupleAlgorithm
findAllTuples = findTuples $ L.fromFoldable $ 
  1 .. (floor $ toNumber numOfOptions / 2.0) >>= setsOfSize

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
      iPeer <- exPeerIndices i
      let action = Tuple iPeer cell
      guard $ effective board action
      pure action

-- | Takes a tuple-finding algorithm and returns a strategy applies the returned
-- | tuples. The enforce-application strategy depends on whether the tuples found are
-- | Naked or Hidden. 
-- |
-- | If the first paramter is true, we assume they are Naked Tuples, otherwise we
-- | assume they are are Hidden Tuples
-- |
enforceTuples :: FindingTupleAlgorithm -> Strategy
enforceTuples tupleFinder puzzle = case maybeActions of
  (Left err) -> Invalid err puzzle
  (Right actions)
    | length actions < 1 -> Stable puzzle
    | otherwise -> Advancing $ batchDropOptions actions <$> puzzle
  where
    board :: Board
    board = snd puzzle

    maybeActions :: Error \/ Array Action
    maybeActions = concat <$> sequence do 
      group <- groups
      pure do
        tuples <- tupleFinder board group
        pure $ concat $ toGroupActions board <$> tuples

-- | enforceTuples specialized with a tuple-finding algorithm that finds
-- | tuples of the given size.
enforceNTuples :: Int -> Strategy
enforceNTuples 1 = enforceNaked1Tuples
enforceNTuples n = enforceTuples $ findNTuples n

-- | enforceTuples specialized with a tuple-finding algorithm that finds
-- | tuples of any/all sizes
enforceAllTuples :: Strategy
enforceAllTuples = enforceTuples findAllTuples 

-- | rollingEnforceHiddenTuples will find all actions in a group,
-- | then apply those actions to the board before finding actions in the next group.
-- |
-- | The hope is that later groups will benefit from the added information created
-- | from earlier groups. Running this strategy with The ST effect might bump performace
-- | a bit more as well. With a board size of 9, this makes at most 26 intermediate 
-- | copies of the board before returning the 27th copy. Sudoku boards are very small
-- | in memory, so the benefit of ST is likely to be extremely small as well.
-- | 
-- | Still, it might be a good learning excersize
-- |
-- | This algorithm checks for both Naked and Hidden Tuples since smaller tuples can be used to
-- | uniformly narrow the search-space for larger tuples. 
-- |
rollingEnforceTuples :: Strategy
rollingEnforceTuples inputPuzzle = foldl 
  enforce (Stable inputPuzzle) groups
  where
    enforce :: Stateful Puzzle -> Group -> Stateful Puzzle
    enforce ip@(Invalid _ _) group = ip
    enforce sop@(Solved _) group = sop
    enforce sPuzzle group = let
     
      puzzle :: Puzzle
      puzzle = unwrapStateful sPuzzle

      board :: Board
      board = snd puzzle

      maybeActions :: Error \/ Array Action
      maybeActions = do
        tuples <- findAllTuples board group
        pure $ concat $ toGroupActions board <$> tuples
      
    in case maybeActions of
      (Left err) -> Invalid err puzzle
      (Right actions) 
        | length actions < 1 -> sPuzzle
        | otherwise -> Advancing $ batchDropOptions actions <$> puzzle

-- | Creates a ladder strategy out of the tuple strategies in this module
-- | See Sudoku.Strategy.Common for an explanation of ladderStrats
ladderTuples :: Strategy
ladderTuples = ladderStrats $ NonEmptyArray
  [ enforceNaked1Tuples -- Very effective early on and runs very fast
  , rollingEnforceTuples -- Optimized, but still pretty heavy, runs last
  ]