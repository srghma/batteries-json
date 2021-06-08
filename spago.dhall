{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign-object"
  , "identity"
  , "integers"
  , "invariant"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "polyform"
  , "polyform-batteries-core"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "record"
  , "record-extra"
  , "strings"
  , "test-unit"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "validation"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
