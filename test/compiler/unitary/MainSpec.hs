{-# LANGUAGE ScopedTypeVariables #-}

module MainSpec (spec) where

import Test.Hspec
import ParseArguments (parseContent, getCladExtension)

spec :: Spec
spec = do
    describe "parseContent - error cases" $ do

        it "returns error when wrong number of arguments" $ do
            let args = ["file1.clad", "file2.clad"]
            result <- parseContent args
            result `shouldBe` Left "wrong number of arguments (expected one .clad file)"

        it "returns error when file has invalid extension" $ do
            let args = ["file.txt"]
            result <- parseContent args
            result `shouldBe` Left "invalid file extension (expected .clad)"

        it "returns error when file does not exist" $ do
            let args = ["nofile.clad"]
            result <- parseContent args
            result `shouldBe` Left "file does not exist: nofile.clad"

        it "returns error when no input file provided" $ do
            let args = []
            result <- parseContent args
            result `shouldBe` Left "no input file provided"

    describe "getCladExtension" $ do
        it "returns True for .clad files" $ do
            getCladExtension "test.clad" `shouldBe` True
            getCladExtension "path/to/file.clad" `shouldBe` True

        it "returns False for non-.clad files" $ do
            getCladExtension "test.txt" `shouldBe` False
            getCladExtension "test" `shouldBe` False
            getCladExtension "test.scheme" `shouldBe` False
