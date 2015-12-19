# haskell-frontmatter
Attoparsec parsers for YAML frontmatter as used in Jekyll. Because of how
`Data.Yaml` is implemented using `Data.Aeson`, the
`Data.Yaml.Frontmatter.frontmatterYaml` instance of `Parser` works with YAML and
JSON.

## Installation
_This actually won't work right now_
```
$ stack install frontmatter
```

## Usage
```haskell
import Data.Frontmatter
import Data.Yaml (Value)

main = ByteString.readFile "something.md" >>= parseYamlFrontmatter >>= \case
    Done ri fm -> do
        print (fm :: Value) -- aeson object in the frontmatter (the explicit
                            -- type required because the parser will return
                            -- anything with a FromJSON

        putStrLn ri         -- rest of the document
    _ -> error "Parse failure"
```

See the haddocks for more information. Essentially exports an `Attoparsec`
parser and some helpers. Usage examples also available at the `test` directory.

## License
This software is published under the MIT license. For more information refer to
the [LICENSE](/LICENSE) file.
