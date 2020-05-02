{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "canvas"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "generics-rep"
  , "orders"
  , "profunctor-lenses"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
