module Stateful where

import Prelude

import Data.Array (foldl)
import Data.Array.NonEmpty (NonEmptyArray, head, tail)
import Error (Error)

data Stateful a =
  Advancing a
  | Stable a
  | Solved a
  | Invalid Error a

derive instance functorStateful :: Functor Stateful

unwrapStateful :: ∀ a. Stateful a -> a
unwrapStateful (Advancing a) = a
unwrapStateful (Stable a) = a
unwrapStateful (Solved a) = a
unwrapStateful (Invalid _ a) = a

isAdvancing :: ∀ a. Stateful a -> Boolean
isAdvancing (Advancing _) = true
isAdvancing _ = false

isStable :: ∀ a. Stateful a -> Boolean
isStable (Stable _) = true
isStable _ = false

isSolved :: ∀ a. Stateful a -> Boolean
isSolved (Solved _) = true
isSolved _ = false

isInvalid :: ∀ a. Stateful a -> Boolean
isInvalid (Invalid _ _) = true
isInvalid _ = false

constructorString :: ∀ a. Stateful a -> String
constructorString (Advancing _) = "Advancing"
constructorString (Stable _) = "Stable"
constructorString (Solved _) = "Solved"
constructorString (Invalid _ _) = "Invalid"

-- | A restricted version of bindFlipped that doesn't allow you to map between types
-- |
-- | This lets Stateful have a lot (but not all) of the power of a monad without 
-- | actually being a monad.
onlyAdvancing :: ∀ a. (a -> Stateful a) -> Stateful a -> Stateful a
onlyAdvancing fn (Advancing v) = fn v
onlyAdvancing _ v = v

-- | Keep running the given function until it returns something not Advancing
notAdvancing :: ∀ a. (a -> Stateful a) -> a -> Stateful a
notAdvancing fn v = notAdvancing' (onlyAdvancing fn) (fn v)

-- | Keep running the given function until it returns something not Advancing
notAdvancing' :: ∀ a. (Stateful a -> Stateful a) -> Stateful a -> Stateful a
notAdvancing' fn v@(Advancing _) = notAdvancing' fn (fn v)
notAdvancing' _ v = v

-- | Take an array of functions that create stateful values and compose them
-- | interleaving the given function
-- | If any function returns an Advancing value, start from the beginning.
repeatAdvancing 
  :: ∀ a. (Stateful a -> Stateful a) 
  -> NonEmptyArray (a -> Stateful a) 
  -> a 
  -> Stateful a
repeatAdvancing notStableFn fns v =
  (Advancing v) # foldl ladderCompose 
    (notAdvancing' $ head metaFns) (tail metaFns)
  where
    ladderCompose 
      :: (Stateful a -> Stateful a) 
      -> (Stateful a -> Stateful a) 
      -> (Stateful a -> Stateful a)
    ladderCompose acc next = notAdvancing' 
      ( acc >>> notStableFn >>> next )

    metaFns :: NonEmptyArray (Stateful a -> Stateful a)
    metaFns = onlyAdvancing <$> fns