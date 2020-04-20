{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "ordered-collections"
  , "psci-support"
  , "random"
  , "spec"
  , "spec-discovery"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
