{-# LANGUAGE OverloadedStrings #-}

module ParseToExprSpec (spec) where

import Test.Hspec
import ParseToExpr
import Types
import Text.Megaparsec (parse)

spec :: Spec
spec = describe "ParseToExpr" $ do

    describe "parseExpr" $ do

        it "parses integer numbers" $ do
            parse parseExpr "" "42" `shouldBe` Right (Number 42)
            parse parseExpr "" "-10" `shouldBe` Right (Number (-10))

        it "parses booleans" $ do
            parse parseExpr "" "#t" `shouldBe` Right (Boolean True)
            parse parseExpr "" "#f" `shouldBe` Right (Boolean False)

        it "parses symbols" $ do
            parse parseExpr "" "foo" `shouldBe` Right (Symbol "foo")
            parse parseExpr "" "+" `shouldBe` Right (Symbol "+")
            parse parseExpr "" "<=" `shouldBe` Right (Symbol "<=")

        it "parses simple lists" $ do
            parse parseExpr "" "(+ 1 2)" `shouldBe`
                Right (List [Symbol "+", Number 1, Number 2])

        it "parses nested lists" $ do
            parse parseExpr "" "(if (< x 10) (* x 3) (div x 2))" `shouldBe`
                Right (List [
                    Symbol "if",
                    List [Symbol "<", Symbol "x", Number 10],
                    List [Symbol "*", Symbol "x", Number 3],
                    List [Symbol "div", Symbol "x", Number 2]
                ])

    describe "parseProgram" $ do

        it "parses multiple expressions" $ do
            let input = "(define foo 42)\n(define bar #t)\n(+ foo 10)"
            parse parseProgram "" input `shouldBe`
                Right [
                    List [Symbol "define", Symbol "foo", Number 42],
                    List [Symbol "define", Symbol "bar", Boolean True],
                    List [Symbol "+", Symbol "foo", Number 10]
                ]

        it "parses empty program" $ do
            parse parseProgram "" "" `shouldBe` Right []

        it "fails on invalid syntax" $ do
            let input = "(+ 1 2"
            case parse parseProgram "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected parse error on unbalanced parentheses"
