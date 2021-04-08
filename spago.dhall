{ name = "halogen-ss-solver"
, dependencies =
  [ "console", "debug", "effect", "halogen", "psci-support", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
