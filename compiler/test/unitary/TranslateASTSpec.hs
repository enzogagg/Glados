{-# LANGUAGE OverloadedStrings #-}

module TranslateASTSpec (spec) where

import Test.Hspec
import Types
import TranslateAST (translateExpr)

spec :: Spec
spec = describe "TranslateAST" $ do

    describe "translateExpr - Basic Forms" $ do

        it "converts Number to IANumber" $ do
            translateExpr (Number 42) `shouldBe` Right (IANumber 42)

        it "converts FloatLiteral to IAFloatLiteral" $ do
            translateExpr (FloatLiteral 3.14) `shouldBe` Right (IAFloatLiteral 3.14)

        it "converts Boolean to IABoolean" $ do
            translateExpr (Boolean True) `shouldBe` Right (IABoolean True)

        it "converts String to IAString" $ do
            translateExpr (String "hello") `shouldBe` Right (IAString "hello")

        it "converts Symbol to IASymbol" $ do
            translateExpr (Symbol "x") `shouldBe` Right (IASymbol "x")

        it "converts simple List to IAList" $ do
            let expr = List [Number 1, Symbol "+"]
            let expected = IAList [IANumber 1, IASymbol "+"]
            translateExpr expr `shouldBe` Right expected

    describe "translateExpr - Special Forms" $ do

        it "translates 'if' with correct structure" $ do
            let expr = List [Symbol "if", Boolean True, Number 1, Number 2]
            let expected = IAIf (IABoolean True) (IANumber 1) (IANumber 2)
            translateExpr expr `shouldBe` Right expected

        it "translates 'lambda' with correct structure" $ do
            let expr = List [Symbol "lambda", List [Symbol "x", Symbol "y"], List [Symbol "+", Symbol "x", Symbol "y"]]
            let expectedBody = IAList [IASymbol "+", IASymbol "x", IASymbol "y"]
            let expected = IALambda ["x", "y"] expectedBody
            translateExpr expr `shouldBe` Right expected

        it "translates variable 'define' (short form)" $ do
            let expr = List [Symbol "define", Symbol "pi", FloatLiteral 3.14]
            let expected = IADefine "pi" (IAFloatLiteral 3.14)
            translateExpr expr `shouldBe` Right expected

        it "translates function 'define' (syntactic sugar)" $ do
            let body = List [Symbol "+", Symbol "a", Symbol "b"]
            let expr = List [Symbol "define", List [Symbol "add", Symbol "a", Symbol "b"], body]

            let expectedLambdaBody = IAList [IASymbol "+", IASymbol "a", IASymbol "b"]
            let expectedLambda = IALambda ["a", "b"] expectedLambdaBody
            let expected = IADefine "add" expectedLambda

            translateExpr expr `shouldBe` Right expected

        it "translates 'quote'" $ do
            let expr = List [Symbol "quote", List [Symbol "a", Number 1]]
            let expectedQuoted = IAList [IASymbol "a", IANumber 1]
            let expected = IAQuote expectedQuoted
            translateExpr expr `shouldBe` Right expected