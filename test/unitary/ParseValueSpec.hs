{-# LANGUAGE OverloadedStrings #-}

module ParseValueSpec (spec) where

import Test.Hspec
import ParseValue
import Types
import TailCallOptimization (runTrampoline)

spec :: Spec
spec = describe "ParseValue" $ do

    describe "evalValue" $ do

        it "evaluates numbers" $ do
            (evalValue (Number 42) [] >>= runTrampoline) `shouldReturn` Right (IntVal 42)

        it "evaluates booleans" $ do
            (evalValue (Boolean True) [] >>= runTrampoline) `shouldReturn` Right (BoolVal True)
            (evalValue (Boolean False) [] >>= runTrampoline) `shouldReturn` Right (BoolVal False)

        it "evaluates symbols from environment" $ do
            let env = [("x", IntVal 10)]
            (evalValue (Symbol "x") env >>= runTrampoline) `shouldReturn` Right (IntVal 10)

        it "fails on unbound symbols" $ do
            (evalValue (Symbol "unknown") [] >>= runTrampoline) `shouldReturn` Left "unbound symbol: unknown"

        it "evaluates if expressions" $ do
            let env = []
            (evalValue (List [Symbol "if", Boolean True, Number 1, Number 2]) env >>= runTrampoline) `shouldReturn` Right (IntVal 1)
            (evalValue (List [Symbol "if", Boolean False, Number 1, Number 2]) env >>= runTrampoline) `shouldReturn` Right (IntVal 2)

        it "evaluates lambda expressions" $ do
            let env = []
            (evalValue (List [Symbol "lambda", List [Symbol "x"], Number 5]) env >>= runTrampoline) `shouldReturn` Right (FuncVal ["x"] (Number 5) [])

        it "evaluates function applications" $ do
            let env = builtins
            (evalValue (List [Symbol "+", Number 2, Number 3]) env >>= runTrampoline) `shouldReturn` Right (IntVal 5)

        it "fails on empty list" $ do
            (evalValue (List []) [] >>= runTrampoline) `shouldReturn` Left "empty list is not a valid expression"

    describe "showValue" $ do

        it "shows integers" $ do
            showValue (IntVal 42) `shouldBe` "42"

        it "shows booleans" $ do
            showValue (BoolVal True) `shouldBe` "#t"
            showValue (BoolVal False) `shouldBe` "#f"

        it "shows functions" $ do
            showValue (FuncVal [] (Number 0) []) `shouldBe` "#<procedure>"
            showValue (Primitive (\_ -> Right (IntVal 0))) `shouldBe` "#<primitive-procedure>"

        it "shows lists" $ do
            showValue (ListVal [IntVal 1, IntVal 2, IntVal 3]) `shouldBe` "(1 2 3)"

        it "shows symbols" $ do
            showValue (SymbolVal "x") `shouldBe` "x"

        it "shows void" $ do
            showValue Void `shouldBe` "#<void>"

    describe "primAdd" $ do
        it "adds two integers" $ do
            primAdd [IntVal 2, IntVal 3] `shouldBe` Right (IntVal 5)
    
    describe "primSub" $ do
        it "subtracts two integers" $ do
            primSub [IntVal 2, IntVal 3] `shouldBe` Right (IntVal (-1))
    
    describe "primMul" $ do
        it "multiplies two integers" $ do
            primMul [IntVal 2, IntVal 3] `shouldBe` Right (IntVal 6)
    
    describe "primDiv" $ do
        it "divides two integers" $ do
            primDiv [IntVal 2, IntVal 3] `shouldBe` Right (IntVal 0)
    
    describe "primMod" $ do
        it "modulates two integers" $ do
            primMod [IntVal 2, IntVal 3] `shouldBe` Right (IntVal 2)
    
    describe "primLt" $ do
        it "returns true if the first integer is less than the second" $ do
            primLt [IntVal 2, IntVal 3] `shouldBe` Right (BoolVal True)
    
    describe "primEq" $ do
        it "returns true if the two integers are equal" $ do
            primEq [IntVal 2, IntVal 2] `shouldBe` Right (BoolVal True)
    
    describe "primCons" $ do
        it "cons a list" $ do
            primCons [IntVal 4, ListVal [IntVal 1, IntVal 2, IntVal 3]] `shouldBe` Right (ListVal [IntVal 4, IntVal 1, IntVal 2, IntVal 3])
        
    describe "primCar" $ do
        it "returns the first element of a list" $ do
            primCar [ListVal [IntVal 1, IntVal 2, IntVal 3]] `shouldBe` Right (IntVal 1)
    
    describe "primCdr" $ do
        it "returns the rest of the list" $ do
            primCdr [ListVal [IntVal 1, IntVal 2, IntVal 3]] `shouldBe` Right (ListVal [IntVal 2, IntVal 3])
    
    describe "primList" $ do
        it "returns a list" $ do
            primList [IntVal 1, IntVal 2, IntVal 3] `shouldBe` Right (ListVal [IntVal 1, IntVal 2, IntVal 3])
    
    describe "primNull" $ do
        it "returns true if the list is empty" $ do
            primNull [ListVal []] `shouldBe` Right (BoolVal True)
        it "returns false if the list is not empty" $ do
            primNull [ListVal [IntVal 1, IntVal 2, IntVal 3]] `shouldBe` Right (BoolVal False)
        