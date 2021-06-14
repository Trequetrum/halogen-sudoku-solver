-- | A re-implementation of the NTuples module, but using MetaData
-- |
-- | This implementation remembers the results of previous computations and attempts to not re-do
-- | the same work on future iterations. Likely to be pretty helpfull in combination with brute force
-- |
module Sudoku.Strategy.NTupleStrat where

import Prelude

import Data.Array (catMaybes, filter, length, nub, union, (..), (\\))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldl, foldr)
import Data.Map (Map, alter, fromFoldableWith, lookup, singleton, unionWith)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Error (Error)
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Board, batchDropOptions)
import Sudoku.Group (Group, groupIndices, groups)
import Sudoku.Index (Index)
import Sudoku.OSet (OSet, allOptionsSet, dropOptions, isSuperset, noOptionsSet, setOptions, setsOfSize)
import Sudoku.Puzzle (MetaBoard, Puzzle)
import Sudoku.Strategy.Common (Strategy, ladderStrats)
import Sudoku.Strategy.NTuple (NTuple, NTupleType(..), nTupleSize, peerExpand, toGroupActions, toTupleIn)

-- | Given an array of tuples, this updates a metaboard such that every tuple is counted and their 
-- | relavant information is stored to be easily retrived during future iterations.
-- |
updateTupleMeta :: Array NTuple -> MetaBoard -> MetaBoard
updateTupleMeta [] board = board
updateTupleMeta tuples board = board
  { tupleState = unionWith mergeGroup board.tupleState newTupleState
  , tupleCount = foldr 
      (\tuple -> alter (alterCount tuple) (nTupleSize tuple))
      board.tupleCount
      tuples
  }
  where
    mappableNTuple :: NTuple -> Tuple Group (Map Int (Tuple OSet (Array Index)))
    mappableNTuple t@{ group, options, position } =
      Tuple group $ singleton (nTupleSize t) (Tuple options position)

    newTupleState :: Map Group (Map Int (Tuple OSet (Array Index)))
    newTupleState = fromFoldableWith mergeGroup $ mappableNTuple <$> tuples

    mergeGroup ::
         Map Int (Tuple OSet (Array Index))
      -> Map Int (Tuple OSet (Array Index))
      -> Map Int (Tuple OSet (Array Index))
    mergeGroup = unionWith \(Tuple lS lI) (Tuple rS rI) ->
      Tuple (setOptions lS rS) (union lI rI) 

    alterCount :: 
         NTuple
      -> Maybe { naked :: Int, hidden :: Int, both :: Int, gen :: Int } 
      -> Maybe { naked :: Int, hidden :: Int, both :: Int, gen :: Int }
    alterCount tuple Nothing = alterCount tuple $ Just 
      { naked : 0
      , hidden : 0
      , both : 0
      , gen : 0
      }
    alterCount tuple (Just n) = Just case tuple.tupleType of
      Gen ->    n { gen    = n.gen    + 1 }
      Naked ->  n { naked  = n.naked  + 1 }
      Both ->   n { both   = n.both   + 1 }
      Hidden -> n { hidden = n.hidden + 1 }

-- | Gets the options (as encoded in a cell) and the Indices for which tuples have already 
-- | been discovered
-- |
tupleState :: Int -> Group -> MetaBoard -> Tuple OSet (Array Index)
tupleState size group metaboard = 
  foldl foldSizes (Tuple noOptionsSet []) $ taken <$> 1 .. size
  where
    taken :: Int -> Maybe (Tuple OSet (Array Index))
    taken size' = lookup group metaboard.tupleState >>= lookup size'

    foldSizes
      :: (Tuple OSet (Array Index)) 
      -> Maybe (Tuple OSet (Array Index)) 
      -> (Tuple OSet (Array Index))
    foldSizes acc Nothing = acc
    foldSizes (Tuple cell indices) (Just (Tuple nxtCell nxtIndices)) =
      Tuple (setOptions cell nxtCell) (nub $ indices <> nxtIndices)

-- | This is a metaSearch because it leans on reliable metadata to narrow the search-space. 
-- |
-- | On the other hand, since metadata here is stored and retrived between searches, succesive iterations
-- | don't have to re-compute any of the solved search state-space.
-- |
-- | This searches both naked and hidden tuples of the same size at the same time. They both narrow
-- | the search space in the same way, so this saves time on calling this function twice (once for each
-- | type of tuple).
-- |
-- | This accomplishes a lot in one fell swoop. The implementation could probably be tidied up a bit.
-- |
findNTuples :: Puzzle -> Int -> Group -> Error \/ Array NTuple
findNTuples puzzle size group = do
  tuples <- catMaybes <$> sequence ( toTupleIn board group indices <$> setList )
  pure ( tuples >>= peerExpand )
  where
    board :: Board
    board = snd puzzle

    startState :: Tuple OSet (Array Index)
    startState = tupleState size group $ fst puzzle

    indices :: Array Index
    indices = groupIndices group \\ snd startState

    setList :: Array OSet
    setList = filter setListFilter ( setsOfSize size )

    setListFilter :: OSet -> Boolean
    setListFilter = isSuperset $ dropOptions (fst startState) allOptionsSet
    

-- | This applies tuples found in a group immediately before searching the
-- | next group. This is more efficient, though a bit harder to reason about
-- | what "one iteration" of such a strategy is. 
rollingEnforceNTuples :: Int -> Strategy
rollingEnforceNTuples size inputPuzzle = foldl 
  enforce (Stable inputPuzzle) groups
  where
    enforce :: Stateful Puzzle -> Group -> Stateful Puzzle
    enforce ip@(Invalid _ _) _ = ip
    enforce sop@(Solved _) _ = sop
    enforce sPuzzle group = let

      puzzle :: Puzzle
      puzzle = unwrapStateful sPuzzle

      eTuples :: Error \/ Array NTuple
      eTuples = findNTuples puzzle size group

    in case eTuples of
      (Left err) -> Invalid err puzzle
      (Right tuples)
        | length tuples < 1 -> sPuzzle
        | otherwise -> Advancing $ bimap 
          (updateTupleMeta tuples) 
          (batchDropOptions $ tuples >>= toGroupActions (snd puzzle))
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