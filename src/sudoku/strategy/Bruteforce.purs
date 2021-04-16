-- |
-- | Brute Force Guess Selection for Sudoku Puzzles
-- |
-- | This module implements a basic backtracking brute force solution for
-- | Sudoku Puzzles. It tries every available option, but can rely on two
-- | things to improve performance a bit
-- |   * A strategy to narrow the search space before making a guess
-- |   * A selector to chooses which option and cell to try first
-- |
-- | Type for Strategy :: Puzzle -> Stateful Puzzle
-- | Type for Selectors :: Board -> Maybe (Tuple Option Index)
-- |
-- | There are two choices we have to make in implementing the selection: 
-- |   * Variable Ordering: which cell do we try first
-- |   * Value Ordering: which option do we try first for the cell
-- |
module Sudoku.Strategy.Bruteforce where

import Prelude

import Data.Array (findMap, (..))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..), snd)
import Error (Error(..))
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Board, batchDropOptions, findIndex, isSolved, (!!))
import Sudoku.Cell (countOptions, toCell, toggleCell, trustFirstOption)
import Sudoku.Index (Index)
import Sudoku.Option (Option, numOfOptions)
import Sudoku.Puzzle (Puzzle)
import Sudoku.Strategy.Common (Strategy, StatefulStrategy, advanceOrFinish, ladderStrats)
import Sudoku.Strategy.NTuples (enforceHiddenNTuples, enforceNakedNTuples, ladderTuples)

type Selector = Board -> Maybe (Tuple Option Index)

-- | A simple selector
-- |  * Variable Ordering: Pick the first cell with > 1 option
-- |  * Value Ordering: Pick the first option
selectFirst :: Selector
selectFirst board = Tuple <$> option <*> maybeI
  where
    maybeI = findIndex (countOptions >>> (_ > 1)) board
    option = do
      i <- maybeI
      pure $ board !! i # trustFirstOption

-- | A 
-- Variable Ordering: A common heuristic called minimum remaining values, 
--    which means that we choose the (or one of the) cells with the 
--    minimum number of possible options. This increases our odds of 
--    guessing correctly
-- Value Ordering: Pick the first option
selectMinOption :: Selector
selectMinOption board = Tuple <$> option <*> maybeI
  where
    maybeI = findMap 
      (\size -> findIndex (countOptions >>> eq size) board) $ 
      2 .. (numOfOptions - 1)
    option = do
      i <- maybeI
      pure $ board !! i # trustFirstOption

-- | The slowest strategy, but it's guaranteed to solve any board
-- | that is solvable. 
-- |
-- | It tries all the possible options until the board is solved.
-- |
-- | selector: this function tells the backtracking algorithm which 
-- |   option to try next.
-- | strat: this function is a strategy that is applied after each step 
-- |   to narrow down the search space. If, for example, doingNothing 
-- |   is given as a strat, then every possible permutation is tried 
-- |   (This isn't advisable)
backtrackingBruteForce :: Selector -> Strategy -> Strategy
backtrackingBruteForce selector strat = strat >>> bbfRecurse
  where
    bbfRecurse :: StatefulStrategy
    bbfRecurse sop@(Solved _) = sop
    bbfRecurse ip@(Invalid _ _) = ip
    bbfRecurse sp@(Stable _) = bbfRecurse $ advanceOrFinish sp
    bbfRecurse (Advancing puzzle) = 
      if isSolved (snd $ unwrapStateful guessAttempt) || isNothing selectedOption
      then guessAttempt
      else nextPuzzle dropOptionOnPuzzle
      where
        selectedOption :: Maybe (Tuple Option Index)
        selectedOption = selector $ snd puzzle

        guessAttempt :: Stateful Puzzle
        guessAttempt = nextPuzzle $ toggleCell >>> dropOptionOnPuzzle

        nextPuzzle action = case selectedOption of
          Nothing -> Invalid (Error "No Solutions" "This is a puzzle for which no solutions exist") puzzle
          Just (Tuple option index) -> 
            bbfRecurse $ strat $ action (toCell option) index

        dropOptionOnPuzzle option index = batchDropOptions [Tuple index option] <$> puzzle

-- | This is losely the strategy for solving Sudokus outlined in "Solving Every 
-- | Sudoku Puzzle" by Peter Norvig.
norvigBruteForce :: Strategy
norvigBruteForce = backtrackingBruteForce
  selectMinOption $ ladderStrats $ NonEmptyArray
  [ enforceNakedNTuples 1
  , enforceHiddenNTuples 1
  ]

-- | BacktrackingBruteForce that checks for early Naked Tuples and 
-- | all Hidden Tuples
ladderTupleBruteForce :: Strategy
ladderTupleBruteForce = backtrackingBruteForce
  selectMinOption ladderTuples