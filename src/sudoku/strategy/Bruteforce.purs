module Sudoku.Strategy.Bruteforce where

import Prelude

import Data.Array (findIndex, findMap, (!!), (..))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..), snd)
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Common (Board, Cell, Index, Puzzle, StatefulStrategy, Strategy, bNot, batchDropOptions, boardSize, countOptions, firstOption, isSolved, toCell)
import Sudoku.Strategy.Common (advanceOrFinish, ladderStrats)
import Sudoku.Strategy.NTuples (enforceHiddenNTuples, enforceNakedNTuples, ladderTuples)

-------------------------------------------------------------------
-- Brute Force Guess Selection for Sudoku Puzzles
-------------------------------------------------------------------
-- Type for Selectors :: Board -> Maybe (Tuple Option Index)
--
-- There are two choices we have to make in implementing the selection: 
-- * Variable Ordering: which cell do we try first
-- * Value Ordering: which option do we try first for the cell
-------------------------------------------------------------------

-- Variable Ordering: Pick the first cell with > 1 option
-- Value Ordering: Pick the first option
selectFirst :: Board -> Maybe (Tuple Cell Index)
selectFirst board = Tuple <$> option <*> i
  where
    i = findIndex (countOptions >>> (_ > 1)) board
    option = do
      i' <- i
      cell <- board !! i'
      pure $ cell # firstOption >>> toCell

-- Variable Ordering: A common heuristic called minimum remaining values, 
--    which means that we choose the (or one of the) cells with the 
--    minimum number of possible options. This increases our odds of 
--    guessing correctly
-- Value Ordering: Pick the first option
selectMinOption :: Board -> Maybe (Tuple Cell Index)
selectMinOption board = Tuple <$> option <*> i
  where
    i = findMap 
      (\size -> findIndex (countOptions >>> eq size) board) $ 
      2 .. (boardSize - 1)
    option = do
      i' <- i
      cell <- board !! i'
      pure $ cell # firstOption >>> toCell

-- The slowest strategy, but it's guaranteed to solve any board
-- that is solvable. 
-- 
-- It tries all the possible options until the board is solved.
-- 
-- selector: this function tells the backtracking algorithm which 
--    option to try next.
-- strat: this function is a strategy that is applied after each step 
--    to narrow down the search space. If, for example, doingNothing 
--    is given as a strat, then every possible permutation is tried 
--    (This isn't advisable)
backtrackingBruteForce :: 
  (Board -> Maybe (Tuple Cell Index)) ->
  Strategy -> Strategy
backtrackingBruteForce selector strat = strat >>> bbfRecurse
  where
    bbfRecurse :: StatefulStrategy
    bbfRecurse fp@(Finished _) = fp
    bbfRecurse sp@(Stable _) = bbfRecurse $ advanceOrFinish sp
    bbfRecurse (Advancing puzzle) = 
      if isSolved (snd $ unwrapStateful guessAttempt) || isNothing selectedOption
      then guessAttempt
      else nextPuzzle dropOptionOnPuzzle
      where
        selectedOption :: Maybe (Tuple Cell Index)
        selectedOption = selector $ snd puzzle

        guessAttempt :: Stateful Puzzle
        guessAttempt = nextPuzzle $ bNot >>> dropOptionOnPuzzle

        nextPuzzle action = case selectedOption of
          Nothing -> Finished puzzle
          Just (Tuple option index) -> 
            bbfRecurse $ strat $ action option index

        dropOptionOnPuzzle option index = batchDropOptions [Tuple index option] <$> puzzle

-- This is the strategy for solving Sudokus outlined in "Solving Every Sudoku Puzzle" by Peter Norvig.
norvigBruteForce :: Strategy
norvigBruteForce = backtrackingBruteForce
  selectMinOption $ ladderStrats $ NonEmptyArray
  [ enforceNakedNTuples 1
  , enforceHiddenNTuples 1
  ]

ladderTupleBruteForce :: Strategy
ladderTupleBruteForce = backtrackingBruteForce
  selectMinOption ladderTuples