module Stateful where

import Data.Functor

import Error (Error)

data Stateful a =
  Advancing a
  | Stable a
  | Solved a
  | Invalid Error a

derive instance functorStateful :: Functor Stateful

unwrapStateful :: forall a. Stateful a -> a
unwrapStateful (Advancing a) = a
unwrapStateful (Stable a) = a
unwrapStateful (Solved a) = a
unwrapStateful (Invalid _ a) = a

isAdvancing :: forall a. Stateful a -> Boolean
isAdvancing (Advancing _) = true
isAdvancing _ = false

isStable :: forall a. Stateful a -> Boolean
isStable (Stable _) = true
isStable _ = false

isSolved :: forall a. Stateful a -> Boolean
isSolved (Solved _) = true
isSolved _ = false

isInvalid :: forall a. Stateful a -> Boolean
isInvalid (Invalid _ _) = true
isInvalid _ = false

constructorString :: forall a. Stateful a -> String
constructorString (Advancing _) = "Advancing"
constructorString (Stable _) = "Stable"
constructorString (Solved _) = "Solved"
constructorString (Invalid _ _) = "Invalid"