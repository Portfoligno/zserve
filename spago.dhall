let psc =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220620/packages.dhall
        sha256:75dbb7030870982349c4eed33b2459aa8325ceb4f727e277495c87e8e0a1d788

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
