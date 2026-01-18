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
    -- Tests des Terminaux (Littéraux)
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

        it "parses characters" $ do
            let result = parse parseProgramAST "" "'c'"
            result `shouldBe` Right (IAProgram [IAChar 'c'])

        it "parses booleans (vrai/faux)" $ do
            let result = parse parseProgramAST "" "vrai"
            result `shouldBe` Right (IAProgram [IABoolean True])
            let result' = parse parseProgramAST "" "faux"
            result' `shouldBe` Right (IAProgram [IABoolean False])

        it "parses lists" $ do
            let result = parse parseProgramAST "" "[1, 2, 3]"
            result `shouldBe` Right (IAProgram [IAList [IANumber 1, IANumber 2, IANumber 3]])
            let resultEmpty = parse parseProgramAST "" "[]"
            resultEmpty `shouldBe` Right (IAProgram [IAList []])

        it "parses tuples" $ do
            let result = parse parseProgramAST "" "(1, vrai)"
            result `shouldBe` Right (IAProgram [IATuple [IANumber 1, IABoolean True]])

        it "parses unit (neant)" $ do
            let result = parse parseProgramAST "" "neant"
            result `shouldBe` Right (IAProgram [IAUnit])

        it "parses symbols" $ do
            let result = parse parseProgramAST "" "my_variable"
            result `shouldBe` Right (IAProgram [IASymbol "my_variable"])

        it "parses array creation (tableau)" $ do
            let result = parse parseProgramAST "" "tableau(1, 2, 3)"
            result `shouldBe` Right (IAProgram [IACall "tableau" [IANumber 1, IANumber 2, IANumber 3]])

        it "parses map creation (dictionnaire)" $ do
            let result = parse parseProgramAST "" "dictionnaire(\"key\", 1)"
            result `shouldBe` Right (IAProgram [IACall "dictionnaire" [IAString "key", IANumber 1]])

        it "parses struct creation (structure)" $ do
            let result = parse parseProgramAST "" "structure(\"field\", 42)"
            result `shouldBe` Right (IAProgram [IACall "structure" [IAString "field", IANumber 42]])

    -- ====================================================================
    -- Tests de la Précédence des Opérateurs Infixés (IAInfix)
    -- ====================================================================

    describe "Infix Expressions & Precedence" $ do

        it "parses simple addition" $ do
            let expected = IAProgram [IAInfix (IANumber 1) "+" (IANumber 2)]
            parse parseProgramAST "" "1 + 2" `shouldBe` Right expected

        it "respects precedence (* before +)" $ do
            let innerMul = IAInfix (IANumber 2) "*" (IANumber 3)
            let expected = IAProgram [IAInfix (IANumber 1) "+" innerMul]
            parse parseProgramAST "" "1 + 2 * 3" `shouldBe` Right expected

        it "handles chaining of same precedence (left associativity)" $ do
            let innerSub = IAInfix (IANumber 5) "-" (IANumber 2)
            let expected = IAProgram [IAInfix innerSub "+" (IANumber 1)]
            parse parseProgramAST "" "5 - 2 + 1" `shouldBe` Right expected

        it "parses parenthesized expressions" $ do
            let innerAdd = IAInfix (IANumber 1) "+" (IANumber 2)
            let expected = IAProgram [IAInfix innerAdd "*" (IANumber 3)]
            parse parseProgramAST "" "(1 + 2) * 3" `shouldBe` Right expected

    -- ====================================================================
    -- Tests des Instructions CLaD (Déclarations et Fonctions)
    -- ====================================================================

    describe "CLaD Statements" $ do

        it "parses constant declaration" $ do
            let expected = IAProgram [IADeclare "PI" (Just FloatT) (IAFloatLiteral 3.14)]
            parse parseProgramAST "" "constante flottant PI = 3.14" `shouldBe` Right expected

        it "parses assignment" $ do
            let value = IAInfix (IANumber 10) "+" (IANumber 5)
            let expected = IAProgram [IAAssign "x" value]
            parse parseProgramAST "" "x = 10 + 5" `shouldBe` Right expected

        it "parses simple function definition" $ do
            let body = [IAReturn (IAInfix (IASymbol "a") "+" (IASymbol "b"))]
            let params = [("a", Just IntT), ("b", Just IntT)]
            let expected = IAProgram [IAFunctionDef "add" params (Just IntT) body]
            let input = "fonction add(entier a, entier b) : entier retourner a + b fin"
            parse parseProgramAST "" input `shouldBe` Right expected

        it "parses conditional (si/fin)" $ do
            let cond = IAInfix (IASymbol "x") ">" (IANumber 10)
            let bodyThen = [IAReturn (IABoolean True)]
            let expected = IAProgram [IAIf cond bodyThen Nothing]
            let input = "si (x > 10) retourner vrai fin"
            parse parseProgramAST "" input `shouldBe` Right expected

    -- ====================================================================
    -- Tests de Programme Complet (IAProgram)
    -- ====================================================================

    describe "parseProgramAST (Full CLaD Program)" $ do

        it "parses program with declaration and main block" $ do
            let expectedDecl = IADeclare "FOO" (Just IntT) (IANumber 42)
            let expectedMain = IAMain [] [IAReturn (IASymbol "FOO")] 
            let input = "constante entier FOO = 42 principal retourner FOO fin"
            let expected = IAProgram [expectedDecl, expectedMain]
            parse parseProgramAST "" input `shouldBe` Right expected

        it "fails on invalid syntax (unclosed function block)" $ do
            let input = "fonction test(entier a) : entier retourner a"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected parse error on unclosed function block"

        it "fails on invalid syntax (misplaced operator)" $ do
            let input = "constante entier x = +"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected parse error on dangling operator"

        it "fails on invalid syntax" $ do
            let input = "constante x 10 + 5"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected parse error on misplaced expression"

    -- ====================================================================
    -- Tests de Structure et de Logique (Fonctions, Boucles, Conditions)
    -- ====================================================================

    describe "CLaD Structure & Error Handling" $ do

        it "fails when constant declaration misses type" $ do
            let input = "constante X = 10"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected error: Type missing after 'constante'"

        it "fails when constant declaration misses equals sign" $ do
            let input = "constante entier Y 10"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected error: '=' missing in declaration"

        it "fails when function misses return type" $ do
            let input = "fonction test(entier a) retourner a fin"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected error: Return type missing"

        it "fails when function misses argument type" $ do
            let input = "fonction test(entier a, b) : entier retourner a fin"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected error: Argument type missing"

        it "parses simple while loop (tantque)" $ do
            let cond = IAInfix (IASymbol "x") "<" (IANumber 10)
            let body = [IAReturn (IASymbol "x")]
            let expected = IAProgram [IAWhile cond body]
            let input = "tantque (x < 10) retourner x fin"
            parse parseProgramAST "" input `shouldBe` Right expected

        it "fails when while loop is missing 'fin'" $ do
            let input = "tantque (x < 10) retourner x"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected error: 'fin' missing after loop body"

        it "parses C-style for loop (pour)" $ do
            let initExpr = IADeclare "i" (Just IntT) (IANumber 0)
            let condExpr = IAInfix (IASymbol "i") "<" (IANumber 10)
            let incExpr  = IAAssign "i" (IAInfix (IASymbol "i") "+" (IANumber 1)) 
            let body     = [IAReturn (IASymbol "i")]
            let expected = Right (IAProgram [IAFor initExpr condExpr incExpr body])

            let input = "pour (variable entier i = 0 ; i < 10 ; i = i + 1) retourner i fin"
            parseAST input `shouldBe` expected

        it "fails when for loop misses semicolon" $ do
            let input = "pour (variable entier i = 0 i < 10; i = i + 1) retourner i fin"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected error: Semicolon missing in for loop header"

        it "parses multi-clause conditional (si sinon si sinon)" $ do
            let cond1 = IAInfix (IASymbol "x") ">" (IANumber 10)
            let cond2 = IAInfix (IASymbol "x") ">" (IANumber 5)
            let body1 = [IAReturn (IANumber 1)]
            let body2 = [IAReturn (IANumber 2)]
            let bodyElse = [IAReturn (IANumber 3)]
            let elseIfNode = IAIf cond2 body2 (Just bodyElse)
            let expected = IAProgram [IAIf cond1 body1 (Just [elseIfNode])]
            let input = unlines [
                    "si (x > 10)",
                    "  retourner 1",
                    "sinon si (x > 5)",
                    "  retourner 2",
                    "sinon",
                    "  retourner 3",
                    "fin"]
            parse parseProgramAST "" input `shouldBe` Right expected

        it "fails when conditional misses 'fin'" $ do
            let input = "si (x > 10) retourner 1 sinon retourner 0"
            case parse parseProgramAST "" input of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected error: 'fin' missing from conditional block"

    -- ====================================================================
    -- Tests de Type (Liste, Tuples, ...)
    -- ====================================================================

    describe "CLaD Tuples & Multi-type Lists" $ do

        it "parses a simple tuple literal" $ do
            let input = "(42, \"Glados\", vrai)"
            let expected = IAProgram [IATuple [IANumber 42, IAString "Glados", IABoolean True]]
            parseAST input `shouldBe` Right expected

        it "parses a constant tuple declaration with explicit types" $ do
            let input = "constante (entier, phrase) paire = (1, \"un\")"
            let tupleType = TupleT [IntT, StringT]
            let tupleVal = IATuple [IANumber 1, IAString "un"]
            let expected = IAProgram [IADeclare "paire" (Just tupleType) tupleVal]
            parseAST input `shouldBe` Right expected

        it "parses nested tuples" $ do
            let input = "(1, (2, 3))"
            let innerTuple = IATuple [IANumber 2, IANumber 3]
            let expected = IAProgram [IATuple [IANumber 1, innerTuple]]
            parseAST input `shouldBe` Right expected

        it "parses a list of tuples" $ do
            let input = "[(1, vrai), (2, faux)]"
            let t1 = IATuple [IANumber 1, IABoolean True]
            let t2 = IATuple [IANumber 2, IABoolean False]
            let expected = IAProgram [IAList [t1, t2]]
            parseAST input `shouldBe` Right expected

        it "parses a function returning a tuple type" $ do
            let input = "fonction coord() : (entier, entier) retourner (10, 20) fin"
            let retType = TupleT [IntT, IntT]
            let body = [IAReturn (IATuple [IANumber 10, IANumber 20])]
            let expected = IAProgram [IAFunctionDef "coord" [] (Just retType) body]
            parseAST input `shouldBe` Right expected

        it "parses a complex nested list and tuple structure" $ do
            let input = "variable liste<liste<(entier, entier)>> complexe = [[(0,0)]]"
            let innerTupleType = TupleT [IntT, IntT]
            let complexType = ListT (ListT innerTupleType)
            let innerVal = IAList [IATuple [IANumber 0, IANumber 0]]
            let expected = IAProgram [IADeclare "complexe" (Just complexType) (IAList [innerVal])]
            parseAST input `shouldBe` Right expected