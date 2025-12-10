{-# LANGUAGE OverloadedStrings #-}

module ParseToASTSpec (spec) where

import Test.Hspec
import ParseToAST (parseAST)
import CladParser (parseProgramAST)
import Types
import Text.Megaparsec (parse)

spec :: Spec
spec = describe "ParseToAST" $ do

    -- ====================================================================
    -- Tests des Terminaux (Parseurs de CladLexer via parseTerm)
    -- Le parser AST peut analyser directement les littéraux si un seul est présent.
    -- ====================================================================

    describe "CLaD Literals" $ do

        it "parses integer numbers" $ do
            let result = parse parseProgramAST "" "42"
            result `shouldBe` Right (IAProgram [IANumber 42])

        it "parses float numbers" $ do
            let result = parse parseProgramAST "" "3.14"
            result `shouldBe` Right (IAProgram [IAFloatLiteral 3.14])

        it "parses strings" $ do
            let result = parse parseProgramAST "" "\"hello\""
            result `shouldBe` Right (IAProgram [IAString "hello"])

        it "parses booleans (vrai/faux)" $ do
            let result = parse parseProgramAST "" "vrai"
            result `shouldBe` Right (IAProgram [IABoolean True])
            let result' = parse parseProgramAST "" "faux"
            result' `shouldBe` Right (IAProgram [IABoolean False])

    -- ====================================================================
    -- Tests de la Précédence des Opérateurs Infixés (IAInfix)
    -- ====================================================================

    describe "Infix Expressions & Precedence" $ do

        it "parses simple addition" $ do
            let expected = IAProgram [IAInfix (IANumber 1) "+" (IANumber 2)]
            parse parseProgramAST "" "1 + 2" `shouldBe` Right expected

        it "respects precedence (* before +)" $ do
            -- 1 + 2 * 3  =>  IAInfix 1 + (IAInfix 2 * 3)
            let innerMul = IAInfix (IANumber 2) "*" (IANumber 3)
            let expected = IAProgram [IAInfix (IANumber 1) "+" innerMul]
            parse parseProgramAST "" "1 + 2 * 3" `shouldBe` Right expected

        it "handles chaining of same precedence (left associativity)" $ do
            -- 5 - 2 + 1  =>  (5 - 2) + 1
            let innerSub = IAInfix (IANumber 5) "-" (IANumber 2)
            let expected = IAProgram [IAInfix innerSub "+" (IANumber 1)]
            parse parseProgramAST "" "5 - 2 + 1" `shouldBe` Right expected

        it "parses parenthesized expressions" $ do
            -- (1 + 2) * 3  =>  (IAInfix 1 + 2) * 3
            let innerAdd = IAInfix (IANumber 1) "+" (IANumber 2)
            let expected = IAProgram [IAInfix innerAdd "*" (IANumber 3)]
            parse parseProgramAST "" "(1 + 2) * 3" `shouldBe` Right expected

    -- ====================================================================
    -- Tests des Instructions CLaD (Déclarations et Fonctions)
    -- ====================================================================

    describe "CLaD Statements" $ do

        it "parses constant declaration" $ do
            -- constante PI 3.14
            let expected = IAProgram [IADeclare "PI" Nothing (IAFloatLiteral 3.14)]
            parse parseProgramAST "" "constante PI = 3.14" `shouldBe` Right expected

        it "parses assignment" $ do
            -- x = 10 + 5
            let value = IAInfix (IANumber 10) "+" (IANumber 5)
            let expected = IAProgram [IAAssign "x" value]
            parse parseProgramAST "" "x = 10 + 5" `shouldBe` Right expected

        it "parses simple function definition" $ do
            -- fonction add(a, b) retourner a + b fin
            let body = [IAReturn (IAInfix (IASymbol "a") "+" (IASymbol "b"))]
            let params = [("a", Nothing), ("b", Nothing)]
            let expected = IAProgram [IAFunctionDef "add" params Nothing body]

            let input = "fonction add(a, b) retourner a + b fin"
            parse parseProgramAST "" input `shouldBe` Right expected

        it "parses conditional (si/fin)" $ do
            let cond = IAInfix (IASymbol "x") ">" (IANumber 10)
            let bodyThen = [IAReturn (IABoolean True)]
            let expected = IAProgram [IAIf cond bodyThen Nothing]
            let input = "si x > 10 retourner vrai fin"
            parse parseProgramAST "" input `shouldBe` Right expected

    -- ====================================================================
    -- Tests de Programme Complet (IAProgram)
    -- ====================================================================

    describe "parseProgramAST (Full CLaD Program)" $ do

        it "parses program with declaration and main block" $ do
            let input = "constante FOO = 42 principal retourner FOO fin"
            let expectedDecl = IADeclare "FOO" Nothing (IANumber 42)
            let expectedMain = IAMain [IAReturn (IASymbol "FOO")]
            let expected = IAProgram [expectedDecl, expectedMain]
            parse parseProgramAST "" input `shouldBe` Right expected

        it "fails on invalid syntax (unclosed function block)" $ do
            let input = "fonction test(a) retourner a"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected parse error on unclosed function block"

        it "fails on invalid syntax (misplaced operator)" $ do
            let input = "constante x = +"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected parse error on dangling operator"

        it "fails on invalid syntax" $ do
            let input = "constante x 10 + 5"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected parse error on misplaced expression"