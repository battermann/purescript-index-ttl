{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "affjax"
    , "argonaut-codecs"
    , "console"
    , "datetime"
    , "effect"
    , "formatters"
    , "partial"
    , "psci-support"
    , "refs"
    , "spec"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
