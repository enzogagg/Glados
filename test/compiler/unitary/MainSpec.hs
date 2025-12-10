{-# LANGUAGE ScopedTypeVariables #-}

module MainSpec (spec) where

import Test.Hspec
import ParseArguments (parseContent, getCladExtension, debugParse)
-- import ParseValue (parseValue) -- RETIRÉ (temporairement, car désactivé dans ParseArguments.hs)

spec :: Spec
spec = do
    describe "parseContent - error cases" $ do

        it "returns error when wrong number of arguments" $ do
            let args = ["file1.clad", "file2.clad"]
            result <- parseContent args
            -- CHANGÉ: Message d'erreur plus précis
            result `shouldBe` Left "wrong number of arguments (expected one .clad file)"

        it "returns error when file has invalid extension" $ do
            let args = ["file.txt"]
            result <- parseContent args
            -- CHANGÉ: Message d'erreur plus précis
            result `shouldBe` Left "invalid file extension (expected .clad)"

        it "returns error when file does not exist" $ do
            let args = ["nofile.clad"]
            result <- parseContent args
            -- CHANGÉ: Utilisation de .clad
            result `shouldBe` Left "file does not exist: nofile.clad"

        it "returns error when no input file provided" $ do
            let args = []
            result <- parseContent args
            -- CHANGÉ: Gère le cas où aucun argument n'est donné (anciennement REPL)
            result `shouldBe` Left "no input file provided"

    describe "getCladExtension" $ do -- CHANGÉ: Renommé
        it "returns True for .clad files" $ do
            getCladExtension "test.clad" `shouldBe` True
            getCladExtension "path/to/file.clad" `shouldBe` True

        it "returns False for non-.clad files" $ do
            getCladExtension "test.txt" `shouldBe` False
            getCladExtension "test" `shouldBe` False
            getCladExtension "test.scheme" `shouldBe` False

    -- describe "parseValue" $ do
    --     Suite de tests pour parseValue retirée car parseValue a été désactivé
    --     ou nécessitera une refonte complète pour la VM (non pertinent pour l'étape de parsing).