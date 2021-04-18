module Stateful where

import Data.Functor

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