module Sudoku.Strategy.Pointing where

import Prelude

import Data.Array (concat, filter, foldl, length, (\\))
import Data.Bifunctor (bimap)
import Data.Tuple (Tuple(..), fst, snd)
import Stateful (Stateful(..), unwrapStateful)
import Sudoku.Board (Action, batchDropOptions, boardRoot, effective, filterIndices)
import Sudoku.Group (Group, groupIndices, groups, toGroupsIntersection)
import Sudoku.OSet (allOptionsSet, asOptions, dropOptions, hasOption, toOSet)
import Sudoku.Puzzle (Puzzle)
import Sudoku.Strategy.Common (Strategy)
import Sudoku.Strategy.NTupleStrat (tupleState)

pointActions :: Puzzle -> Group -> Tuple Int (Array Action)
pointActions puzzle group = Tuple (length actionsPerOption) (concat actionsPerOption)
  where
    Tuple tOptions tIndices = tupleState boardRoot group (fst puzzle)

    actionsPerOption :: Array (Array Action)
    actionsPerOption = filter (length >>> (_ > 0)) do
      option <- asOptions $ dropOptions tOptions allOptionsSet
      let foundIndices = filterIndices (hasOption option) (snd puzzle) (groupIndices group \\ tIndices)
      pointedGroup <- filter (_ /= group) (toGroupsIntersection foundIndices)
      pure $ filter (effective $ snd puzzle) 
        $ flip Tuple (toOSet option) 
        <$> (groupIndices pointedGroup \\ foundIndices)

-- | This applies tuples found in a group immediately before searching the
-- | next group. This is more efficient, though a bit harder to reason about
-- | what "one iteration" of such a strategy is. 
rollingEnforcePointing :: Strategy
rollingEnforcePointing inputPuzzle = foldl 
  enforce (Stable inputPuzzle) groups
  where
    enforce :: Stateful Puzzle -> Group -> Stateful Puzzle
    enforce ip@(Invalid _ _) _ = ip
    enforce sop@(Solved _) _ = sop
    enforce sPuzzle group = let

      puzzle :: Puzzle
      puzzle = unwrapStateful sPuzzle

      Tuple hits actions = pointActions puzzle group

    in if length actions > 0 
      then Advancing $ bimap
        (\mb -> mb { pointing = mb.pointing + hits})
        (batchDropOptions actions)
        puzzle
      else sPuzzle