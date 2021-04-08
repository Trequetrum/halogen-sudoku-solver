module Utility where

import Prelude

import Data.Array (filter, find, length, nub, nubEq, unsafeIndex, (!!))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple, fst, snd, swap)
import Partial.Unsafe (unsafePartial)

foreign import modifyPerIndex_impl :: forall a.
  (forall b c. Tuple b c -> b) ->
  (forall b c. Tuple b c -> c) ->
  Array (Tuple Int (a -> a)) -> 
  Array a -> Array a

modifyPerIndex :: forall a. Array (Tuple Int (a -> a)) -> Array a -> Array a
modifyPerIndex = modifyPerIndex_impl fst snd

foreign import dropMaskPerIndex_impl ::
  (forall b c. Tuple b c -> b) ->
  (forall b c. Tuple b c -> c) ->
  Array (Tuple Int Int) -> 
  Array Int -> Array Int

dropMaskPerIndex :: Array (Tuple Int Int) -> Array Int -> Array Int
dropMaskPerIndex = dropMaskPerIndex_impl fst snd

{-----------------
 - Utility Functions
 ----------------}

-- Return true of both predicates are true for the same value
both :: forall a. (a -> Boolean) -> (a -> Boolean) -> a -> Boolean
both p1 p2 v = p1 v && p2 v

-- Return true if every element in the array is unique
allUnique :: forall a. Ord a => Array a -> Boolean
allUnique xs = length xs == (length $ nub xs)

-- Return true if every element in the array is unique
allUniqueEq :: forall a. Eq a => Array a -> Boolean
allUniqueEq xs = length xs == (length $ nubEq xs)

indexOn :: forall a b. (a -> b) -> Array a -> Int -> Maybe b
indexOn fn arr i = fn <$> arr !! i

-- Unsafe array index. Only use this if you're sure the index is defined
indexx :: forall a. Array a -> Int -> a
indexx = unsafePartial $ unsafeIndex

unsafeIndexOn :: forall a b. (a -> b) -> Array a -> Int -> b
unsafeIndexOn fn arr i = fn $ indexx arr i

{--------------
-- Droping the more general utility function since performance is improved 
-- When an array (stead of foldable) is passed in for actions
---------------
modifyPerIndex :: forall t a. Foldable t => t (Tuple Int (a -> a)) -> Array a -> Array a
modifyPerIndex foldableActions array = run do
  mutableArray <- thaw array
  let actions = fromFoldable foldableActions
  for_ actions \(Tuple index action) -> modify index action mutableArray
  pure mutableArray
-}

{--------------
-- Dropping this in favor of a FFI implemention in javascript 
---------------
modifyPerIndex :: forall a. Array (Tuple Int (a -> a)) -> Array a -> Array a
modifyPerIndex actions array = run do
  mutableArray <- thaw array
  foreach actions \(Tuple index action) -> void $ modify index action mutableArray
  pure mutableArray
-}

keyToInt :: forall a. Tuple String a -> Maybe (Tuple Int a)
keyToInt = swap >>> map fromString >>> sequence >>> map swap

filterUnless :: forall a. (a -> Boolean) -> (a -> Boolean) -> Array a -> Maybe (Array a)
filterUnless filterPred terminatePred array
  | isNothing $ find terminatePred array = Just $ filter filterPred array
  | otherwise = Nothing

selectFilter :: forall a. (a -> Boolean) -> Array Int -> Array a -> Array Int
selectFilter pred indices arr = filter (\i -> maybe false pred $ arr !! i) indices

justWhen :: forall a. (a -> Boolean) -> a -> Maybe a
justWhen pred v = if pred v then Just v else Nothing

inc :: Number -> Number
inc = (_ + 1.0)