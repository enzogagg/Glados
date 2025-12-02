{-# LANGUAGE ScopedTypeVariables #-}

module MainSpec (spec) where

import Test.Hspec
import ParseArguments (parseContent)

spec :: Spec
spec = describe "parseContent - error cases" $ do

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
