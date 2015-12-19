{-# LANGUAGE ScopedTypeVariables #-}
module Data.Frontmatter
    ( -- * Frontmatter parser
      frontmatter
    , frontmatterYaml
      -- * Utility functions
    , parseFrontmatter
    , parseFrontmatterMaybe
    , parseFrontmatterEither
    , parseYamlFrontmatter
    , parseYamlFrontmatterMaybe
    , parseYamlFrontmatterEither
      -- * Re-exports
    , parse
    , maybeResult
    , eitherResult
    , Parser
    , Result
    , IResult(..)
    )
  where

import           Data.Attoparsec.ByteString
import           Data.ByteString            (ByteString)
import           Data.Frontmatter.Internal
import           Data.Yaml                  (FromJSON)
import           Data.Yaml.Frontmatter

parseFrontmatter = parse frontmatter
parseFrontmatterMaybe = maybeResult . parse frontmatter
parseFrontmatterEither = eitherResult . parse frontmatter

parseYamlFrontmatter :: FromJSON a => ByteString -> Result a
parseYamlFrontmatter = parse frontmatterYaml

parseYamlFrontmatterMaybe :: FromJSON a => ByteString -> Maybe a
parseYamlFrontmatterMaybe = maybeResult . parse frontmatterYaml

parseYamlFrontmatterEither :: FromJSON a => ByteString -> Either String a
parseYamlFrontmatterEither = eitherResult . parse frontmatterYaml
