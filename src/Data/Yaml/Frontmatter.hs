{-# LANGUAGE ScopedTypeVariables #-}
module Data.Yaml.Frontmatter where

import           Data.Attoparsec.ByteString
import           Data.Frontmatter.Internal
import           Data.Yaml                  (FromJSON, Value, decodeEither', ParseException (InvalidYaml), YamlException (YamlParseException, yamlProblem, yamlProblemMark, yamlContext), YamlMark (yamlLine, yamlColumn))
import           Data.Yaml.Aeson            (prettyPrintParseException)

-- |
-- Parses a YAML frontmatter or JSON frontmatter from a 'ByteString' as a
-- 'Value'. Because of how @Data.Yaml@ is implemented using @aeson@, this will
-- succeed for JSON frontmatters as well as YAML ones.
frontmatterYaml :: FromJSON a => Parser a
frontmatterYaml = frontmatterYaml' <?> "frontmatterYaml"
  where
    frontmatterYaml' = do
        f <- frontmatter
        case decodeEither' f of
            Left (InvalidYaml (Just (e0@YamlParseException { yamlProblem = msg, yamlContext = ctx, yamlProblemMark = pos }))) ->
              let
                fixedMark =
                  -- Column and line are both incremented, so they count from 1, as is usual...
                  pos {
                    -- ... and an extra increment for the --- marker
                    yamlLine = yamlLine pos + 2,
                    yamlColumn = yamlColumn pos + 1
                  }
                e = InvalidYaml (Just e0 { yamlProblemMark = fixedMark })
              in failYaml e
            Left e -> failYaml e
            Right v -> return v
    failYaml = fail . prettyPrintParseException
