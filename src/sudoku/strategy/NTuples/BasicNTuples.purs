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
import Data.Int (floor, toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Error (Error(..))
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Action, Board, batchDropOptions, filterIndices, indexedCells, effective)
import Sudoku.OSet (OSet, allSets, allOptionsSet, setsOfSize, countOptions, dropOptions, isSuperset, notDisjoint)
import Sudoku.OSet as OSets
import Sudoku.Group (Group, asIdString, exPeerIndices, groupIndices, groups)
import Sudoku.Index (Index)
import Sudoku.Option (numOfOptions)
import Sudoku.Puzzle (Puzzle)
import Sudoku.Strategy.Common (Strategy, ladderStrats)
import Sudoku.Strategy.NTuple (NTuple, NTupleType(..), toGroupActions)

type FindingTupleAlgorithm = Board -> Group -> Maybe (Array NTuple)

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
-- |
-- | tupleRel :: (Cell -> Cell -> Boolean) - if tupleRel (possible cell) (board cell) == true, then 
-- |    (board cell) is counted
-- | illegalRel :: (Int -> Int -> Boolean) - if 
-- |    illegalRel (countOptions possible cell) (count board cell) == true, then we s short circuit
-- |    and return Nothing. Indicating we have a board that isn't legal
-- |
findTuplesByPred :: (OSet -> OSet -> Boolean) -> (Int -> Int -> Boolean) -> 
  List OSet -> NTupleType -> FindingTupleAlgorithm
findTuplesByPred tupleRel illegalRel optionCombinations tupleType board group = 
  find allOptionsSet optionCombinations (groupIndices group) (Just [])
  where
    find :: OSet -> List OSet -> Array Index ->  
      Maybe (Array NTuple) -> Maybe (Array NTuple)
    find _ _ _ Nothing = Nothing
    find _ Nil _ jt = jt
    find options (currComb : restCombs) subgroup jt@(Just tuples)
      | not (OSets.isValid options) = jt
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
          (Just $ tuples <> (pure $ 
            { tupleType
            , group
            , options : currComb 
            , position : matches
            }
          ))
        else if illegalRel (countOptions currComb) (length matches)
        then Nothing
        else find options restCombs subgroup jt

-- | findTuplesByPred specialized to find Naked N Tuples
-- |  * Int: is the size of tuples to look for
findNakedNTuples :: Int -> FindingTupleAlgorithm
findNakedNTuples size = findTuplesByPred isSuperset (<) (L.fromFoldable $ setsOfSize size) Naked

-- | findTuplesByPred specialized to find Hidden N Tuples
-- |  * Int: is the size of tuples to look for
findHiddenNTuples :: Int -> FindingTupleAlgorithm
findHiddenNTuples size = findTuplesByPred notDisjoint (>) (L.fromFoldable $ setsOfSize size) Hidden

-- | findTuplesByPred specialized to find Naked Tuples
findNakedTuples :: FindingTupleAlgorithm
findNakedTuples = findTuplesByPred isSuperset (<) allSets Naked

-- | findTuplesByPred specialized to find Hidden Tuples
findHiddenTuples :: FindingTupleAlgorithm
findHiddenTuples = findTuplesByPred notDisjoint (>) allSets Hidden

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
enforceTuples findTuples puzzle = case maybeActions of
  Nothing -> Invalid (Error "Impossible Tuple" "enforceTuples found an impossible tuple") puzzle
  Just actions 
    | length actions < 1 -> Stable puzzle
    | otherwise -> Advancing $ batchDropOptions actions <$> puzzle
  where
    board :: Board
    board = snd puzzle

    maybeActions :: Maybe (Array Action)
    maybeActions = concat <$> sequence do 
      group <- groups
      pure do
        tuples <- findTuples board group
        pure $ concat $ toGroupActions board <$> tuples
          

-- | enforceNakedTuples' specialized with a tuple-finding algorithm that finds
-- | tuples of any/all sizes
enforceNakedTuples :: Strategy
enforceNakedTuples = enforceTuples findNakedTuples

-- | enforceNakedTuples' specialized with a tuple-finding algorithm that finds
-- | tuples of the given size. Defers 1-Tuples to the specialised 
-- | enforceNaked1Tuples implementation
enforceNakedNTuples :: Int -> Strategy
enforceNakedNTuples 1 = enforceNaked1Tuples
enforceNakedNTuples n = enforceTuples $ findNakedNTuples n

-- | enforceHiddenTuples' specialized with a tuple-finding algorithm that finds
-- | tuples of any/all sizes
enforceHiddenTuples :: Strategy
enforceHiddenTuples = enforceTuples findHiddenTuples 

-- | enforceHiddenTuples' specialized with a tuple-finding algorithm that finds
-- | tuples of the given size.
enforceHiddenNTuples :: Int -> Strategy
enforceHiddenNTuples = findHiddenNTuples >>> enforceTuples

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
  , enforceHiddenTuples    -- Optimized, but still pretty heavy, runs last
  ]

-- | List of Cells whose size are less than half the number of options. 
flooredSmallCells :: List OSet
flooredSmallCells = L.fromFoldable $ 1 .. (floor $ toNumber numOfOptions / 2.0) >>= setsOfSize

-- | findTuplesByPred specialized to find Naked Tuples
findSmallNakedTuples :: FindingTupleAlgorithm
findSmallNakedTuples = findTuplesByPred isSuperset (<) flooredSmallCells Naked

-- | findTuplesByPred specialized to find Hidden Tuples
findSmallHiddenTuples :: FindingTupleAlgorithm
findSmallHiddenTuples = findTuplesByPred notDisjoint (>) flooredSmallCells Hidden

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
  enforce
  (Stable inputPuzzle)
  groups
  where
    enforce :: Stateful Puzzle -> Group -> Stateful Puzzle
    enforce ip@(Invalid _ _) group = ip
    enforce sop@(Solved _) group = sop
    enforce sPuzzle group = let
     
      puzzle :: Puzzle
      puzzle = unwrapStateful sPuzzle

      board :: Board
      board = snd puzzle

      maybeActions :: Maybe (Array Action)
      maybeActions = do
        nakedTuples <- findSmallNakedTuples board group
        hiddenTuples <- findSmallHiddenTuples board group
        let nakedActions = concat $ toGroupActions board <$> nakedTuples
        let hiddenActions = concat $ toGroupActions board <$> hiddenTuples
        pure $ nakedActions <> hiddenActions 
      
    in case maybeActions of
      Nothing -> Invalid 
        (Error "Impossible Tuple" $ "rollingEnforceTuples found an impossible tuple in " <> asIdString group) 
        puzzle
      Just actions 
        | length actions < 1 -> sPuzzle
        | otherwise -> Advancing $ batchDropOptions actions <$> puzzle

