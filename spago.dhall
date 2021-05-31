{ name = "halogen-ss-solver"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "safe-coerce"
  , "spec"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
