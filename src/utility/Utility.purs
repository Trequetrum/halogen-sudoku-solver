module Utility where

import Prelude

import Data.Array (deleteBy, filter, find, foldr, length, nub, nubEq, unsafeIndex, (!!))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple, fst, snd, swap)
import Effect.Aff (Aff, delay)
import Partial.Unsafe (unsafePartial)

foreign import modifyPerIndex_impl :: ∀ a.
  (∀ b c. Tuple b c -> b) ->
  (∀ b c. Tuple b c -> c) ->
  Array (Tuple Int (a -> a)) -> 
  Array a -> Array a

modifyPerIndex :: ∀ a. Array (Tuple Int (a -> a)) -> Array a -> Array a
modifyPerIndex = modifyPerIndex_impl fst snd

foreign import dropMaskPerIndex_impl ::
  (∀ b c. Tuple b c -> b) ->
  (∀ b c. Tuple b c -> c) ->
  Array (Tuple Int Int) -> 
  Array Int -> Array Int

dropMaskPerIndex :: Array (Tuple Int Int) -> Array Int -> Array Int
dropMaskPerIndex = dropMaskPerIndex_impl fst snd

{-----------------
 - Utility Functions
 ----------------}

-- Return true of both predicates are true for the same value
both :: ∀ a. (a -> Boolean) -> (a -> Boolean) -> a -> Boolean
both p1 p2 v = p1 v && p2 v

-- Return true if every element in the array is unique
allUnique :: ∀ a. Ord a => Array a -> Boolean
allUnique xs = length xs == (length $ nub xs)

-- Return true if every element in the array is unique
allUniqueEq :: ∀ a. Eq a => Array a -> Boolean
allUniqueEq xs = length xs == (length $ nubEq xs)

indexOn :: ∀ a b. (a -> b) -> Array a -> Int -> Maybe b
indexOn fn arr i = fn <$> arr !! i

-- Unsafe array index. Only use this if you're sure the index is defined
indexx :: ∀ a. Array a -> Int -> a
indexx = unsafePartial $ unsafeIndex

unsafeIndexOn :: ∀ a b. (a -> b) -> Array a -> Int -> b
unsafeIndexOn fn arr i = fn $ indexx arr i

{--------------
-- Droping the more general utility function since performance is improved 
-- When an array (stead of foldable) is passed in for actions
---------------
modifyPerIndex :: ∀ t a. Foldable t => t (Tuple Int (a -> a)) -> Array a -> Array a
modifyPerIndex foldableActions array = run do
  mutableArray <- thaw array
  let actions = fromFoldable foldableActions
  for_ actions \(Tuple index action) -> modify index action mutableArray
  pure mutableArray
-}

{--------------
-- Dropping this in favor of a FFI implemention in javascript 
---------------
modifyPerIndex :: ∀ a. Array (Tuple Int (a -> a)) -> Array a -> Array a
modifyPerIndex actions array = run do
  mutableArray <- thaw array
  foreach actions \(Tuple index action) -> void $ modify index action mutableArray
  pure mutableArray
-}

keyToInt :: ∀ a. Tuple String a -> Maybe (Tuple Int a)
keyToInt = swap >>> map fromString >>> sequence >>> map swap

filterUnless :: ∀ a. (a -> Boolean) -> (a -> Boolean) -> Array a -> Maybe (Array a)
filterUnless filterPred terminatePred array
  | isNothing $ find terminatePred array = Just $ filter filterPred array
  | otherwise = Nothing

selectFilter :: ∀ a. (a -> Boolean) -> Array Int -> Array a -> Array Int
selectFilter pred indices arr = filter (\i -> maybe false pred $ arr !! i) indices

justWhen :: ∀ a. (a -> Boolean) -> a -> Maybe a
justWhen pred v = if pred v then Just v else Nothing

inc :: Number -> Number
inc = (_ + 1.0)

affFn :: forall a b. (a -> b) -> a -> Aff b
affFn fn a = do
  delay $ Milliseconds 0.0
  pure $ fn a

differenceBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
differenceBy eq = foldr (deleteBy eq)