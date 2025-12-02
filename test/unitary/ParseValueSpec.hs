{-# LANGUAGE OverloadedStrings #-}

module ParseValueSpec (spec) where

import Test.Hspec
import ParseValue
import Types

spec :: Spec
spec = describe "ParseValue" $ do

    describe "evalValue" $ do

        it "evaluates numbers" $ do
            evalValue (Number 42) [] `shouldBe` Right (IntVal 42)

        it "evaluates booleans" $ do
            evalValue (Boolean True) [] `shouldBe` Right (BoolVal True)
            evalValue (Boolean False) [] `shouldBe` Right (BoolVal False)

        it "evaluates symbols from environment" $ do
            let env = [("x", IntVal 10)]
            evalValue (Symbol "x") env `shouldBe` Right (IntVal 10)

        it "fails on unbound symbols" $ do
            evalValue (Symbol "unknown") [] `shouldBe` Left "unbound symbol: unknown"

        it "evaluates if expressions" $ do
            let env = []
            evalValue (List [Symbol "if", Boolean True, Number 1, Number 2]) env `shouldBe` Right (IntVal 1)
            evalValue (List [Symbol "if", Boolean False, Number 1, Number 2]) env `shouldBe` Right (IntVal 2)

        it "evaluates lambda expressions" $ do
            let env = []
            evalValue (List [Symbol "lambda", List [Symbol "x"], Number 5]) env `shouldBe` Right (FuncVal ["x"] (Number 5) [])

        it "evaluates function applications" $ do
            let env = builtins
            evalValue (List [Symbol "+", Number 2, Number 3]) env `shouldBe` Right (IntVal 5)

        it "fails on empty list" $ do
            evalValue (List []) [] `shouldBe` Left "empty list is not a valid expression"

    describe "showValue" $ do

        it "shows integers" $ do
            showValue (IntVal 42) `shouldBe` "42"

        it "shows booleans" $ do
            showValue (BoolVal True) `shouldBe` "#t"
            showValue (BoolVal False) `shouldBe` "#f"

        it "shows functions" $ do
            showValue (FuncVal [] (Number 0) []) `shouldBe` "#<procedure>"
            showValue (Primitive (\_ -> Right (IntVal 0))) `shouldBe` "#<primitive-procedure>"

        it "shows void" $ do
            showValue Void `shouldBe` "#<void>"