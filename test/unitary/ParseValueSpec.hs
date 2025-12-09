{-# LANGUAGE OverloadedStrings #-}

module ParseValueSpec (spec) where

import Test.Hspec
import ParseValue (builtins, primAdd, primSub, primMul, primDiv, primMod, primLt, primEq, primCons, primCar, primCdr, primList, primNull, evalValue, showValue) -- Imports from ParseValue
import Types
import TranslateAST (translateExpr)
import TailCallOptimization (runTrampoline)

shouldBeApprox :: Either String Value -> Either String Value -> Expectation
shouldBeApprox (Right (FloatVal a)) (Right (FloatVal b)) =
    abs (a - b) `shouldSatisfy` (< 0.0001)
shouldBeApprox a b = a `shouldBe` b

evalExprTest :: Expr -> Env -> IO (Either String Value)
evalExprTest expr env =
    case translateExpr expr of
        Left err -> return (Left err)
        Right iast -> evalValue iast env >>= runTrampoline

spec :: Spec
spec = describe "ParseValue" $ do

    describe "evalValue (via evalExprTest)" $ do

        it "evaluates numbers" $ do
            evalExprTest (Number 42) [] `shouldReturn` Right (IntVal 42)

        it "evaluates floats" $ do
            result <- evalExprTest (FloatLiteral 3.14) []
            result `shouldBeApprox` Right (FloatVal 3.14)

        it "evaluates strings" $ do
            evalExprTest (String "hello") [] `shouldReturn` Right (StringVal "hello")

        it "evaluates booleans" $ do
            evalExprTest (Boolean True) [] `shouldReturn` Right (BoolVal True)
            evalExprTest (Boolean False) [] `shouldReturn` Right (BoolVal False)

        it "evaluates symbols from environment" $ do
            let env = [("x", IntVal 10)]
            evalExprTest (Symbol "x") env `shouldReturn` Right (IntVal 10)

        it "fails on unbound symbols" $ do
            evalExprTest (Symbol "unknown") [] `shouldReturn` Left "unbound symbol: unknown"

        it "evaluates if expressions" $ do
            let env = []
            evalExprTest (List [Symbol "if", Boolean True, Number 1, Number 2]) env `shouldReturn` Right (IntVal 1)
            evalExprTest (List [Symbol "if", Boolean False, Number 1, Number 2]) env `shouldReturn` Right (IntVal 2)

        it "evaluates lambda expressions" $ do
            let env = []
            evalExprTest (List [Symbol "lambda", List [Symbol "x"], Number 5]) env
                `shouldReturn` Right (FuncVal ["x"] (IANumber 5) [])

        it "evaluates function applications" $ do
            let env = builtins
            evalExprTest (List [Symbol "+", Number 2, Number 3]) env `shouldReturn` Right (IntVal 5)

        it "evaluates quote expressions" $ do
            let env = []
            evalExprTest (List [Symbol "quote", Symbol "x"]) env `shouldReturn` Right (SymbolVal "x")
            evalExprTest (List [Symbol "quote", List [Symbol "a", Symbol "b"]]) env
                `shouldReturn` Right (ListVal [SymbolVal "a", SymbolVal "b"])

        it "fails on if with non-boolean condition" $ do
            let env = []
            evalExprTest (List [Symbol "if", Number 1, Number 2, Number 3]) env `shouldReturn` Left "if condition must be a boolean"

        it "fails on calling non-function" $ do
            let env = [("x", IntVal 5)]
            evalExprTest (List [Symbol "x", Number 1]) env `shouldReturn` Left "attempt to call a non-function value"

        it "fails on lambda with non-symbol parameters" $ do
            let env = []
            evalExprTest (List [Symbol "lambda", List [String "not-symbol"], Number 5]) env `shouldReturn` Left "expected symbol"

        it "fails on function with wrong number of arguments" $ do
            let env = [("f", FuncVal ["x", "y"] (IANumber 5) [])]
            evalExprTest (List [Symbol "f", Number 1]) env `shouldReturn` Left "function expects 2 arguments, got 1"

        it "fails on function application with error in argument" $ do
            let env = builtins
            evalExprTest (List [Symbol "+", Symbol "unknown", Number 3]) env `shouldReturn` Left "unbound symbol: unknown"

        it "fails on empty list" $ do
            evalExprTest (List []) [] `shouldReturn` Left "empty list is not a valid function call"

        it "evaluates user-defined functions" $ do
            let bodyIAST = IAList [IASymbol "+", IASymbol "a", IASymbol "b"]
            let env = [("add", FuncVal ["a", "b"] bodyIAST builtins)]
            evalExprTest (List [Symbol "add", Number 10, Number 20]) env `shouldReturn` Right (IntVal 30)

        it "fails on if with error in condition" $ do
            let env = []
            evalExprTest (List [Symbol "if", Symbol "unknown", Number 1, Number 2]) env `shouldReturn` Left "unbound symbol: unknown"

    describe "exprToValue via quote tests" $ do
        it "converts Number to IntVal" $ do
            evalExprTest (List [Symbol "quote", Number 42]) [] `shouldReturn` Right (IntVal 42)

        it "converts FloatLiteral to FloatVal" $ do
            result <- evalExprTest (List [Symbol "quote", FloatLiteral 3.14]) []
            result `shouldBeApprox` Right (FloatVal 3.14)

        it "converts Boolean to BoolVal" $ do
            evalExprTest (List [Symbol "quote", Boolean True]) [] `shouldReturn` Right (BoolVal True)

        it "converts String to StringVal" $ do
            evalExprTest (List [Symbol "quote", String "test"]) [] `shouldReturn` Right (StringVal "test")

    describe "extractSymbol (Error check via lambda)" $ do
        it "fails on non-symbol in lambda parameters" $ do
            evalExprTest (List [Symbol "lambda", List [Number 1], Number 5]) [] `shouldReturn` Left "expected symbol"


    describe "showValue" $ do

        it "shows integers" $ do
            showValue (IntVal 42) `shouldBe` "42"

        it "shows booleans" $ do
            showValue (BoolVal True) `shouldBe` "#t"
            showValue (BoolVal False) `shouldBe` "#f"

        it "shows functions" $ do
            showValue (FuncVal [] (IANumber 0) []) `shouldBe` "#<procedure>" -- ⚠️ IANumber 0
            showValue (Primitive (\_ -> Right (IntVal 0))) `shouldBe` "#<primitive-procedure>"

        it "shows lists" $ do
            showValue (ListVal [IntVal 1, IntVal 2, IntVal 3]) `shouldBe` "(1 2 3)"

        it "shows symbols" $ do
            showValue (SymbolVal "x") `shouldBe` "x"

        it "shows void" $ do
            showValue Void `shouldBe` "#<void>"

        it "shows strings" $ do
            showValue (StringVal "test") `shouldBe` "test"

        it "shows floats" $ do
            showValue (FloatVal 3.14) `shouldBe` "3.14"


    describe "primAdd" $ do
        it "adds two integers" $ do
            primAdd [IntVal 2, IntVal 3] `shouldBe` Right (IntVal 5)
        it "adds two floats" $ do
            primAdd [FloatVal 2.5, FloatVal 3.5] `shouldBeApprox` Right (FloatVal 6.0)
        it "fails with wrong number of arguments" $ do
            primAdd [IntVal 2] `shouldBe` Left "+ requires two integer arguments"

    describe "primSub" $ do
        it "subtracts two integers" $ do
            primSub [IntVal 2, IntVal 3] `shouldBe` Right (IntVal (-1))
        it "subtracts two floats" $ do
            primSub [FloatVal 5.5, FloatVal 2.5] `shouldBeApprox` Right (FloatVal 3.0)
        it "fails with wrong arguments" $ do
            primSub [IntVal 2] `shouldBe` Left "- requires two integer arguments"

    describe "primMul" $ do
        it "multiplies two integers" $ do
            primMul [IntVal 2, IntVal 3] `shouldBe` Right (IntVal 6)
        it "multiplies two floats" $ do
            primMul [FloatVal 2.5, FloatVal 4.0] `shouldBeApprox` Right (FloatVal 10.0)
        it "fails with wrong arguments" $ do
            primMul [IntVal 2] `shouldBe` Left "* requires two integer arguments"

    describe "primDiv" $ do
        it "divides two integers" $ do
            primDiv [IntVal 6, IntVal 3] `shouldBe` Right (IntVal 2)
        it "divides two floats" $ do
            primDiv [FloatVal 10.0, FloatVal 2.0] `shouldBeApprox` Right (FloatVal 5.0)
        it "fails on division by zero (int)" $ do
            primDiv [IntVal 5, IntVal 0] `shouldBe` Left "division by zero"
        it "fails on division by zero (float)" $ do
            primDiv [FloatVal 5.0, FloatVal 0.0] `shouldBe` Left "division by zero"
        it "fails with wrong arguments" $ do
            primDiv [IntVal 2] `shouldBe` Left "div requires two integer arguments"

    describe "primMod" $ do
        it "modulates two integers" $ do
            primMod [IntVal 7, IntVal 3] `shouldBe` Right (IntVal 1)
        it "fails on modulo by zero (int)" $ do
            primMod [IntVal 5, IntVal 0] `shouldBe` Left "modulo by zero"
        it "fails on modulo by zero (float)" $ do
            primMod [FloatVal 5.0, FloatVal 0.0] `shouldBe` Left "modulo by zero"
        it "fails with wrong arguments" $ do
            primMod [IntVal 2] `shouldBe` Left "mod requires two integer arguments"

    describe "primLt" $ do
        it "returns true if the first integer is less than the second" $ do
            primLt [IntVal 2, IntVal 3] `shouldBe` Right (BoolVal True)
        it "returns false if the first integer is not less than the second" $ do
            primLt [IntVal 5, IntVal 3] `shouldBe` Right (BoolVal False)
        it "compares floats" $ do
            primLt [FloatVal 2.5, FloatVal 3.5] `shouldBe` Right (BoolVal True)
        it "returns false for floats" $ do
            primLt [FloatVal 5.5, FloatVal 3.5] `shouldBe` Right (BoolVal False)
        it "fails with wrong arguments" $ do
            primLt [IntVal 2] `shouldBe` Left "< requires two integer arguments"

    describe "primEq" $ do
        it "returns true if the two integers are equal" $ do
            primEq [IntVal 2, IntVal 2] `shouldBe` Right (BoolVal True)
        it "returns false if the two integers are not equal" $ do
            primEq [IntVal 2, IntVal 3] `shouldBe` Right (BoolVal False)
        it "compares floats" $ do
            primEq [FloatVal 2.5, FloatVal 2.5] `shouldBe` Right (BoolVal True)
        it "returns false for different floats" $ do
            primEq [FloatVal 2.5, FloatVal 3.5] `shouldBe` Right (BoolVal False)
        it "compares booleans" $ do
            primEq [BoolVal True, BoolVal True] `shouldBe` Right (BoolVal True)
        it "returns false for different booleans" $ do
            primEq [BoolVal True, BoolVal False] `shouldBe` Right (BoolVal False)
        it "fails with wrong argument types" $ do
            primEq [IntVal 2] `shouldBe` Left "eq? requires two arguments of the same type"

    describe "primCons" $ do
        it "cons a list" $ do
            primCons [IntVal 4, ListVal [IntVal 1, IntVal 2, IntVal 3]] `shouldBe` Right (ListVal [IntVal 4, IntVal 1, IntVal 2, IntVal 3])
        it "fails with non-list second argument" $ do
            primCons [IntVal 1, IntVal 2] `shouldBe` Left "cons requires a list as the second argument"
        it "fails with wrong number of arguments" $ do
            primCons [IntVal 1] `shouldBe` Left "cons requires two arguments"

    describe "primCar" $ do
        it "returns the first element of a list" $ do
            primCar [ListVal [IntVal 1, IntVal 2, IntVal 3]] `shouldBe` Right (IntVal 1)
        it "fails on empty list" $ do
            primCar [ListVal []] `shouldBe` Left "car of empty list"
        it "fails on non-list" $ do
            primCar [IntVal 1] `shouldBe` Left "car requires a list"
        it "fails with wrong number of arguments" $ do
            primCar [] `shouldBe` Left "car requires one argument"

    describe "primCdr" $ do
        it "returns the rest of the list" $ do
            primCdr [ListVal [IntVal 1, IntVal 2, IntVal 3]] `shouldBe` Right (ListVal [IntVal 2, IntVal 3])
        it "fails on empty list" $ do
            primCdr [ListVal []] `shouldBe` Left "cdr of empty list"
        it "fails on non-list" $ do
            primCdr [IntVal 1] `shouldBe` Left "cdr requires a list"
        it "fails with wrong number of arguments" $ do
            primCdr [] `shouldBe` Left "cdr requires one argument"

    describe "primList" $ do
        it "returns a list" $ do
            primList [IntVal 1, IntVal 2, IntVal 3] `shouldBe` Right (ListVal [IntVal 1, IntVal 2, IntVal 3])

    describe "primNull" $ do
        it "returns true if the list is empty" $ do
            primNull [ListVal []] `shouldBe` Right (BoolVal True)
        it "returns false if the list is not empty" $ do
            primNull [ListVal [IntVal 1, IntVal 2, IntVal 3]] `shouldBe` Right (BoolVal False)
        it "fails on non-list" $ do
            primNull [IntVal 1] `shouldBe` Left "null? requires a list"
        it "fails with wrong number of arguments" $ do
            primNull [] `shouldBe` Left "null? requires one argument"