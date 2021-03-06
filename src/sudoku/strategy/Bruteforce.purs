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
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff)
import Error (Error(..))
import Stateful (Stateful(..), notAdvancing)
import Sudoku.Board (Board, batchDropOptions, findIndex, (!!))
import Sudoku.Index (Index)
import Sudoku.OSet (OSet, countOptions, toOSet, toggleOSet, trustFirstOption)
import Sudoku.Option (Option, numOfOptions)
import Sudoku.Puzzle (Puzzle)
import Sudoku.Strategy.Common (StatefulStrategy, Strategy, advanceOrFinish, ladderStrats)
import Sudoku.Strategy.NTupleStrat (ladderOrder, rollingEnforceNTuples)
import Sudoku.Strategy.Pointing (rollingEnforcePointing)
import Utility (affFn)

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

-- | A Variable Ordering: A common heuristic called minimum remaining values, 
-- | which means that we choose the (or one of the) cells with the 
-- | minimum number of possible options. This increases our odds of 
-- | guessing correctly
-- | Value Ordering: Pick the first option
selectMinOption :: Selector
selectMinOption board = Tuple <$> option <*> maybeI
  where
    maybeI = findMap 
      (\size -> findIndex (countOptions >>> eq size) board) $ 
      2 .. numOfOptions
    option = do
      i <- maybeI
      pure $ board !! i # trustFirstOption

updateBFMeta :: Boolean -> Puzzle -> Puzzle
updateBFMeta success puzzle = case success of
  true  -> lmap (\mb -> mb { bruteForce { guessed = mb.bruteForce.guessed + 1 } }) puzzle
  false -> lmap (\mb -> mb { bruteForce { backtrack = mb.bruteForce.backtrack + 1 } }) puzzle

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
      case guessAttempt, selectedOption of
        ga@(Solved _), _ -> ga
        ga@(Invalid _ _), Nothing -> ga
        _, _ -> updateBFMeta false <$> nextPuzzle dropOptionOnPuzzle
      where
        selectedOption :: Maybe (Tuple Option Index)
        selectedOption = selector $ snd puzzle

        guessAttempt :: Stateful Puzzle
        guessAttempt = updateBFMeta true <$> (nextPuzzle $ toggleOSet >>> dropOptionOnPuzzle)

        nextPuzzle :: (OSet -> Index -> Puzzle) -> Stateful Puzzle
        nextPuzzle action = case selectedOption of
          Nothing -> Invalid 
            ( Error "No Solutions" 
              "After exhaustive search, this is a puzzle for which no solutions exist"
            ) puzzle
          Just (Tuple option index) -> 
            bbfRecurse $ advanceOrFinish $ strat $ action (toOSet option) index

        dropOptionOnPuzzle :: OSet -> Index -> Puzzle
        dropOptionOnPuzzle option index = batchDropOptions [Tuple index option] <$> puzzle

-- | This is losely the strategy for solving Sudokus outlined in "Solving Every 
-- | Sudoku Puzzle" by Peter Norvig.
norvigBruteForce :: Strategy
norvigBruteForce = backtrackingBruteForce
  selectMinOption (notAdvancing $ rollingEnforceNTuples 1)

-- | Ladder various strategies together into a single strategy.
-- | Currently uses all the tupleStrats as well as the pointing strat
ladderAllStrats :: Strategy
ladderAllStrats = ladderStrats (ladderOrder <> pure rollingEnforcePointing)

-- | BacktrackingBruteForce that narrows the search space with all available strategies
-- | between guesses. 
ladderBruteForce :: Strategy
ladderBruteForce = backtrackingBruteForce
  selectMinOption ladderAllStrats

-- | Brute Force that delays after each guess and delegated future work
-- | to the event loop. This stops this computation from monopolising the main thread
-- |
affBacktrackingBruteForce :: Selector -> Strategy -> Puzzle -> Aff (Stateful Puzzle)
affBacktrackingBruteForce selector strat = strat >>> bbfRecurse
  where
    bbfRecurse :: Stateful Puzzle -> Aff (Stateful Puzzle)
    bbfRecurse sop@(Solved _) = pure sop
    bbfRecurse ip@(Invalid _ _) = pure ip
    bbfRecurse sp@(Stable _) = bbfRecurse $ advanceOrFinish sp
    bbfRecurse (Advancing puzzle) = do
      guess <- guessAttempt
      case guess, selectedOption of
        ga@(Solved _), _ -> pure ga
        ga@(Invalid _ _), Nothing -> pure ga
        _, _ -> do
          nxt <- nextPuzzle dropOptionOnPuzzle
          pure $ updateBFMeta false <$> nxt
      where
        selectedOption :: Maybe (Tuple Option Index)
        selectedOption = selector $ snd puzzle

        guessAttempt :: Aff (Stateful Puzzle)
        guessAttempt = do
          nxt <- nextPuzzle (toggleOSet >>> dropOptionOnPuzzle)
          pure $ updateBFMeta true <$> nxt

        nextPuzzle :: (OSet -> Index -> Puzzle) -> Aff (Stateful Puzzle)
        nextPuzzle action = case selectedOption of
          Nothing -> pure $ Invalid 
            ( Error "No Solutions" 
              "After exhaustive search, this is a puzzle for which no solutions exist"
            ) puzzle
          Just (Tuple option index) -> 
            (affFn strat) (action (toOSet option) index) >>= bbfRecurse

        dropOptionOnPuzzle :: OSet -> Index -> Puzzle
        dropOptionOnPuzzle option index = batchDropOptions [Tuple index option] <$> puzzle

affLadderBruteForce :: Puzzle -> Aff (Stateful Puzzle)
affLadderBruteForce = affBacktrackingBruteForce
  selectMinOption ladderAllStrats
