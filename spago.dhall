{ name = "fifth-element"
, dependencies =
  [ "behaviors"
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
