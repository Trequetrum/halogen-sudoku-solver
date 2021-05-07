-- | Strategies for Sudoku Puzzles
-- |
-- | This module provides basic utilities for dealing wth sudoku strategies
-- |
-- | Strategy: A function that takes a board as input and returns 
-- |     a stateful board as output. It should never add options to 
-- |     the returned board, (what it does internally doesn't matter). 
-- | *# Advancing: A strategy returns an advancing board if it has 
-- |     removed at least one option from the board.
-- | *# Stable: A strategy returns a stable board if it has made no 
-- |     changes to the board.
-- | *# Finished: A strategy returns a finished board if it has 
-- |     concluded that a board is either solved or impossible to 
-- |     solve 
-- |     Because strategies should not add options to a board,
-- |     any board that is in an illegal/invalid state is finished
module Sudoku.Strategy.Common where

import Prelude

import Data.Array (foldl)
import Data.Array.NonEmpty (NonEmptyArray, head, tail)
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple (snd)
import Effect.Aff (Aff)
import Error (Error(..))
import Stateful (Stateful(..), isInvalid, isSolved, onlyAdvancing, repeatAdvancing, unwrapStateful)
import Sudoku.Board (isSolvedIffValid, isValid')
import Sudoku.Puzzle (Puzzle)
import Utility (affFn)

type Strategy = Puzzle -> Stateful Puzzle

type StatefulStrategy = Stateful Puzzle -> Stateful Puzzle

-------------------------------------------------------------------
-- Strategies for Sudoku Puzzles
-------------------------------------------------------------------

-- | The strategy of doing nothing. This can never alter a board, 
-- | so it always returns a Stable Board
doingNothing :: Strategy
doingNothing = Stable

-- | This is a StatefulStrategy that checks if a Puzzle is solved or invalid
stayOrFinish :: StatefulStrategy
stayOrFinish statefulPuzzle =
  if isSolved statefulPuzzle || isInvalid statefulPuzzle
  then statefulPuzzle
  else if isJust isValidError
  then Invalid (fromMaybe (Error "" "") isValidError) puzzle
  else if isSolvedIffValid board
  then Solved puzzle
  else statefulPuzzle
  where
    puzzle = unwrapStateful statefulPuzzle
    board = snd puzzle
    isValidError = isValid' board

-- | This is a StatefulStrategy that checks if a Puzzle is solved or invalid and 
-- | never returns a Stable Puzzle. This can be used to compose onlyAdvancing
-- | strategies 
advanceOrFinish :: StatefulStrategy
advanceOrFinish p = case stayOrFinish p of
  (Stable a) -> Advancing a
  notStable -> notStable

-- | This is a meta-strategy that creates a new strategy out of an ordered
-- | list of strategies. The idea is that you order your strategies
-- | from fastest to slowest, then only perform a slower strategy if
-- | all the faster strategies are stable.
-- |
-- | If a slower strategy has made a change, then re-run the faster 
-- | strategies on this new board before repeating. AdvanceOrFinish is
-- | used to check if a strategy has returned a finished board so strategies
-- | do not need to check if they've solved a board.
ladderStrats :: NonEmptyArray Strategy -> Strategy
ladderStrats = repeatAdvancing advanceOrFinish

-------------------------------------------------------------------
-- Experimental Asynchronous Strategies
-------------------------------------------------------------------

affUntilStableMeta :: (Stateful Puzzle -> Aff (Stateful Puzzle)) -> Stateful Puzzle -> Aff (Stateful Puzzle)
affUntilStableMeta strat p@(Advancing _) = strat p >>= affUntilStableMeta strat 
affUntilStableMeta _ p = pure p

affLadderStrats :: NonEmptyArray Strategy -> Puzzle -> Aff (Stateful Puzzle)
affLadderStrats strats puzzle = (Advancing puzzle) # foldl ladderbind 
    (affUntilStableMeta $ head metaStrats) (tail metaStrats)
  where
    ladderbind 
      :: (Stateful Puzzle -> Aff (Stateful Puzzle)) 
      -> (Stateful Puzzle -> Aff (Stateful Puzzle))
      -> Stateful Puzzle -> Aff (Stateful Puzzle)
    ladderbind acc next puzz = affUntilStableMeta (\p -> acc p >>= (advanceOrFinish >>> next)) puzz

    metaStrats :: NonEmptyArray (Stateful Puzzle -> Aff (Stateful Puzzle))
    metaStrats = (onlyAdvancing >>> affFn) <$> strats
