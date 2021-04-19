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
import Error (Error(..))
import Stateful (Stateful(..), isInvalid, isSolved, unwrapStateful)
import Sudoku.Board (isSolvedIffValid, isValid')
import Sudoku.Puzzle (Puzzle)

type Strategy = Puzzle -> Stateful Puzzle

type StatefulStrategy = Stateful Puzzle -> Stateful Puzzle

-------------------------------------------------------------------
-- Strategies for Sudoku Puzzles
-------------------------------------------------------------------

-- | The strategy of doing nothing. This can never alter a board, 
-- | so it always returns a Stable Board
doingNothing :: Strategy
doingNothing = Stable

-- | Turn a Strategy into a StatefulStrategy that will only work on
-- | Advancing puzzles. Other puzzles are left unchanged
onlyAdvancing :: Strategy -> StatefulStrategy
onlyAdvancing strat (Advancing puzzle) = strat puzzle
onlyAdvancing _ statefulPuzzle = statefulPuzzle

-- | This is a StatefulStrategy that checks if a Puzzle is solved or invalid and 
-- | never returns a Stable Puzzle. This can be used to compose onlyAdvancing
-- | strategies 
advanceOrFinish :: StatefulStrategy
advanceOrFinish p = case stayOrFinish p of
  (Stable a) -> Advancing a
  notStable -> notStable

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

-- | Take a Strategy and repeat it until the strategy returns a stable/finished board
untilStable :: Strategy -> Strategy
untilStable strat puzzle = untilStableMeta (onlyAdvancing strat) (strat puzzle)

-- | Take a StatefulStrategy and repeat it until the strategy returns a stable/finished board
untilStableMeta :: StatefulStrategy -> StatefulStrategy
untilStableMeta strat p@(Advancing _) = untilStableMeta strat $ strat p
untilStableMeta _ p = p

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
ladderStrats strats puzzle =
  (Advancing puzzle) # foldl reducer seedValue restStrats
  where
    reducer :: StatefulStrategy -> StatefulStrategy -> StatefulStrategy
    reducer acc next = untilStableMeta (acc >>> next) >>> advanceOrFinish

    metaStrats :: NonEmptyArray StatefulStrategy
    metaStrats = onlyAdvancing <$> strats

    seedValue :: StatefulStrategy
    seedValue = (untilStableMeta $ head metaStrats) >>> advanceOrFinish

    restStrats :: Array StatefulStrategy
    restStrats = tail metaStrats