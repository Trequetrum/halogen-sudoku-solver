-- | A re-implementation of the NTuples module, but using MetaData
-- |
-- | This implementation remembers the results of previous computations and attempts to not re-do
-- | the same work on future iterations. Likely to be pretty helpfull in combination with brute force
-- |
module Sudoku.Strategy.MetaNTuples where

import Prelude

import Data.Array (catMaybes, filter, intersectBy, length, nub, union, (..), (\\))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Map (Map, alter, lookup, singleton)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Error (Error(..))
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Board, batchDropOptions, filterIndices)
import Sudoku.OSet (OSet, allOptionsSet, countOptions, dropOptions, isSuperset, noOptionsSet, notDisjoint, setOptions, setsOfSize)
import Sudoku.Group (Group, groupIndices, groups)
import Sudoku.Index (Index)
import Sudoku.Puzzle (MetaBoard, Puzzle)
import Sudoku.Strategy.Common (Strategy, ladderStrats)
import Sudoku.Strategy.NTuple (NTuple, NTupleType(..), nTupleSize, peerExpand, sameTuple, toError, toGroupActions)
import Utility (differenceBy)

-- | Given an array of tuples, this updates a metaboard such that every tuple is counted and their 
-- | relavant information is stored to be easily retrived during future iterations.
-- |
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

    alterState :: NTuple -> Maybe (Map Int (Tuple OSet (Array Index))) -> Maybe (Map Int (Tuple OSet (Array Index)))
    alterState tuple Nothing = Just $ singleton (nTupleSize tuple) (Tuple tuple.options tuple.position)
    alterState tuple (Just sizeMap) = Just $ alter (alterSizeMap tuple) (nTupleSize tuple) sizeMap

    alterSizeMap :: NTuple -> Maybe (Tuple OSet (Array Index)) -> Maybe (Tuple OSet (Array Index))
    alterSizeMap tuple Nothing = Just $ Tuple tuple.options tuple.position
    alterSizeMap tuple (Just (Tuple optns posns)) = Just $ Tuple (setOptions optns tuple.options) (union posns tuple.position)

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
findNTuples :: Puzzle -> Int -> Group -> Either Error (Array NTuple)
findNTuples puzzle size group = do
  n <- onlyNakedTuples
  h <- onlyHiddenTuples
  s <- bothTypeTuples
  pure $ (n <> h <> s) >>= peerExpand (snd puzzle)
  where
    board :: Board
    board = snd puzzle

    startState :: Tuple OSet (Array Index) 
    startState = tupleState size group $ fst puzzle

    options :: OSet
    options = dropOptions (fst startState) allOptionsSet

    indices :: Array Index
    indices = groupIndices group \\ snd startState

    combs :: Array OSet
    combs = filter (isSuperset options) $ setsOfSize size

    allNakedTuples :: Either Error (Array NTuple)
    allNakedTuples = catMaybes <$> (sequence $ checkFor Naked <$> combs)

    allHiddenTuples :: Either Error (Array NTuple)
    allHiddenTuples = catMaybes <$> (sequence $ checkFor Hidden <$> combs)

    onlyNakedTuples :: Either Error (Array NTuple)
    onlyNakedTuples = differenceBy sameTuple <$> allNakedTuples <*> allHiddenTuples

    onlyHiddenTuples :: Either Error (Array NTuple)
    onlyHiddenTuples = differenceBy sameTuple <$> allHiddenTuples <*> allNakedTuples

    bothTypeTuples :: Either Error (Array NTuple)
    bothTypeTuples = map (\t -> t {tupleType = Both}) <$> (intersectBy sameTuple <$> allNakedTuples <*> allHiddenTuples)

    checkFor :: NTupleType -> OSet -> Either Error (Maybe NTuple)
    checkFor tupleType = case tupleType of
      Naked -> checkIf Naked isSuperset (<)
      Hidden -> checkIf Hidden notDisjoint (>)
      Both -> \c -> do
        n <- checkFor Naked c
        h <- checkFor Hidden c
        case n, h of
          (Just nT), (Just hT) -> 
            if sameTuple nT hT 
            then Right $ Just $ nT {tupleType = Both} 
            else Right Nothing
          _, _ -> Right Nothing
      Gen -> \_ -> Left $
        Error "Illegal Operation" 
        "It is not possible to check for a generated n tuple, did you mean 'Naked'?"
    
    checkIf :: NTupleType -> 
      (OSet -> OSet -> Boolean) -> 
      (Int -> Int -> Boolean) -> 
      OSet -> Either Error (Maybe NTuple)
    checkIf tupleType tupleRel illegalRel comb = 
      if isFailure
      then Left $ toError tuple
      else if isSuccess
      then Right (Just tuple)
      else Right Nothing
      where
        matches = filterIndices (tupleRel comb) board indices
        isSuccess = countOptions comb == length matches
        isFailure = countOptions comb `illegalRel` length matches
        tuple = 
          { tupleType: tupleType
          , group
          , options : comb 
          , position : matches
          }

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