module App.Update where

data Update a = New a | Update a

unwrapUpdate :: forall a. Update a -> a
unwrapUpdate (New v)    = v
unwrapUpdate (Update v) = v