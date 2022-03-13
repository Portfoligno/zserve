let psc =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220303/packages.dhall
        sha256:d7cbc15ea16768e4a4f99baa58a54559dd2648c6c1362de2469d9e41c23b28c3

in  { name = ""
    , dependencies =
      [ "aff"
      , "argonaut-codecs"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "console"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "httpure"
      , "lists"
      , "maybe"
      , "node-child-process"
      , "node-fs"
      , "node-process"
      , "optparse"
      , "prelude"
      , "tuples"
      , "unsafe-coerce"
      ]
    , packages = psc
    , sources = [ "src/main/purescript/**/*.purs" ]
    }
