module Test.DirtyMains where

import Data.Array (mapWithIndex)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, append, discard, pure, show, unit, ($), (<>))
import Sudoku.Format (statefulPuzzleToOptionsString, statefulPuzzleToString)
import Sudoku.Puzzle (Puzzle, fromString)
import Sudoku.Strategy.Bruteforce (ladderTupleBruteForce)
import Sudoku.Strategy.NTuples (enforceNakedNTuples)
import Test.Basic.Data (dummyPuzzle, hardestBoardStringsX11)

solveHardest :: Effect Unit
solveHardest = traverse_ solve $ mapWithIndex Tuple hardestBoardStringsX11

step :: Puzzle -> Effect Unit
step board = do
  pure unit

  log $ statefulPuzzleToOptionsString $
    enforceNakedNTuples 1 $ 
    board

solve :: Tuple Int String -> Effect Unit
solve (Tuple i strng) = do
  pure unit -- Turn our computation into a thunk

  log $ append (show i <> ": ") $
    statefulPuzzleToString $ 
    ladderTupleBruteForce $ 
    fromRight dummyPuzzle $ 
    fromString strng