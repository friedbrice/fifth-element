{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "behaviors"
  , "canvas"
  , "console"
  , "effect"
  , "enums"
  , "event"
  , "foldable-traversable"
  , "generics-rep"
  , "orders"
  , "profunctor-lenses"
  , "psci-support"
  , "random"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
