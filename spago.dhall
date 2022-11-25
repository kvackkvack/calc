{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "generics-rep"
  , "halogen"
  , "js-timers"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "psci-support"
  , "strings"
  , "transformers"
  , "unicode"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
