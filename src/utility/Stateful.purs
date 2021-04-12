module Stateful where

import Data.Functor

data Stateful a =
  Advancing a
  | Stable a
  | Finished a

derive instance functorStateful :: Functor Stateful

unwrapStateful :: forall a. Stateful a -> a
unwrapStateful (Advancing a) = a
unwrapStateful (Stable a) = a
unwrapStateful (Finished a) = a

isAdvancing :: forall a. Stateful a -> Boolean
isAdvancing (Advancing _) = true
isAdvancing _ = false

isStable :: forall a. Stateful a -> Boolean
isStable (Stable _) = true
isStable _ = false

isFinished :: forall a. Stateful a -> Boolean
isFinished (Finished _) = true
isFinished _ = false