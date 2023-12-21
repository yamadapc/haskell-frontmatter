{-# LANGUAGE OverloadedStrings #-}
module Data.FrontmatterSpec where

import qualified Data.ByteString  as ByteString
import           Data.Frontmatter
import           Data.Text        (Text)
import           Data.Yaml
import           Test.Hspec

spec :: Spec
spec = do
    describe "frontmatter :: Parser ByteString" $ do
        it "handles input with no frontmatter and fails returning the doc" $ do
            input <- ByteString.readFile "./no-frontmatter.md"
            let Fail input' _ _ = parse frontmatter input
            input' `shouldBe` input

        it "doesn't fail if the frontmatter is empty" $ do
            input <- ByteString.readFile "./empty-frontmatter.md"
            let Done ri fm = parse frontmatter input
            ri `shouldBe` "# This is a markdown file\nWith an empty frontmatter section.\n"
            fm `shouldBe` ""

        it "returns whatever is in the frontmatter if it's there" $ do
            input <- ByteString.readFile "./text-frontmatter.md"
            let Done ri fm = parse frontmatter input
            ri `shouldBe` "Rest of the input\n"
            fm `shouldBe` "I'm in the frontmatter\n"

    describe "frontmatterYaml :: Parser Value" $ do
        it "parses YAML on the frontmatter" $ do
            input <- ByteString.readFile "./yaml-frontmatter.md"
            let Done ri fm = parse frontmatterYaml input
            ri `shouldBe` "Rest of the input\n"
            fm `shouldBe` object [ "title" .= ("Testing" :: Text)
                                 , "description" .= ("Here" :: Text)
                                 ]

        it "handles JSON" $ do
            input <- ByteString.readFile "./json-frontmatter.md"
            let Done ri fm = parse frontmatterYaml input
            ri `shouldBe` "etc etc\n"
            fm `shouldBe` object [ "complete" .= ("junk" :: Text) ]

        it "handles input with invalid frontmatters" $ do
            input <- ByteString.readFile "./invalid-yaml-frontmatter.md"
            let Fail input' _ctx msg = parse frontmatterYaml input :: Result Value
            input' `shouldBe` "etc etc\n"
            -- ignoring _ctx. Does not appear useful.
            -- A somewhat different message may be ok, but the line and column must be the same
            msg `shouldBe` "Failed reading: YAML parse exception at line 2, column 11:\nmapping values are not allowed in this context"
