module Sudoku.Strategy.Common where

import Prelude

import Data.Array (foldl)
import Data.Array.NonEmpty (NonEmptyArray, head, tail)
import Data.Tuple (snd)
import Stateful (Stateful(..))
import Sudoku.Common (StatefulStrategy, Strategy, isSolvedOrInvalid)

-------------------------------------------------------------------
-- Strategies for Sudoku Puzzles
-------------------------------------------------------------------

-- The strategy of doing nothing. This can never alter a board, 
-- so it always returns a Stable Board
doingNothing :: Strategy
doingNothing = Stable

-- Turn a Strategy into a StatefulStrategy that will only work on
-- Advancing puzzles. Other puzzles are left unchanged
onlyAdvancing :: Strategy -> StatefulStrategy
onlyAdvancing strat (Advancing puzzle) = strat puzzle
onlyAdvancing _ statefulPuzzle = statefulPuzzle

-- This is a StatefulStrategy that checks if a Puzzle is solved and 
-- never returns a Stable Puzzle. This can be used to compose onlyAdvancing
-- strategies 
advanceOrFinish :: StatefulStrategy
advanceOrFinish ap@(Advancing puzzle)
  | isSolvedOrInvalid (snd puzzle) = Finished puzzle
  | otherwise = ap
advanceOrFinish (Stable puzzle)
  | isSolvedOrInvalid (snd puzzle) = Finished puzzle
  | otherwise = Advancing puzzle
advanceOrFinish fp@(Finished puzzle) = fp

-- Take a Strategy and repeat it until the strategy returns a stable/finished board
untilStable :: Strategy -> Strategy
untilStable strat puzzle = untilStableMeta (onlyAdvancing strat) (strat puzzle)

-- Take a StatefulStrategy and repeat it until the strategy returns a stable/finished board
untilStableMeta :: StatefulStrategy -> StatefulStrategy
untilStableMeta strat p@(Advancing _) = untilStableMeta strat $ strat p
untilStableMeta _ p = p

-- This is a meta-strategy that creates a new strategy out of an ordered
-- list of strategies. The idea is that you order your strategies
-- from fastest to slowest, then only perform a slower strategy if
-- all the faster strategies are stable.
--
-- If a slower strategy has made a change, then re-run the faster 
-- strategies on this new board before repeating. AdvanceOrFinish is
-- used to check if a strategy has returned a finished board so strategies
-- do not need to check if they've solved a board.
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