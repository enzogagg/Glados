{-# LANGUAGE ScopedTypeVariables #-}

module MainSpec (spec) where

import Test.Hspec
import ParseArguments (parseContent, getScmExtension, parseArgs, handleInput)
import ParseValue (parseValue)
import Types

spec :: Spec
spec = do
    describe "parseContent - error cases" $ do

        it "returns error when wrong number of arguments" $ do
            let args = ["file1.scm", "file2.scm"]
            result <- parseContent args
            result `shouldBe` Left "wrong number of arguments"

        it "returns error when file has invalid extension" $ do
            let args = ["file.txt"]
            result <- parseContent args
            result `shouldBe` Left "invalid type file"

        it "returns error when file does not exist" $ do
            let args = ["nofile.scm"]
            result <- parseContent args
            result `shouldBe` Left "file does not exist: nofile.scm"

    describe "getScmExtension" $ do
        it "returns True for .scm files" $ do
            getScmExtension "test.scm" `shouldBe` True
            getScmExtension "path/to/file.scm" `shouldBe` True

        it "returns False for non-.scm files" $ do
            getScmExtension "test.txt" `shouldBe` False
            getScmExtension "test" `shouldBe` False
            getScmExtension "test.scheme" `shouldBe` False

    describe "parseValue" $ do
        it "returns error on empty program" $ do
            result <- parseValue []
            result `shouldBe` Left "empty program"
