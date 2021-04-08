module Error where

import Prelude

data Error = Error String String

instance showError :: Show Error where
  show (Error errName errMessage) = 
    "(Error " <> show errName <> " " <> show errMessage <> ")"

instance eqError :: Eq Error where
  eq (Error lName lMessage) (Error rName rMessage) = 
    lName == rName && lMessage == rMessage

name :: Error -> String
name (Error n _) = n

message :: Error -> String
message (Error _ m) = m