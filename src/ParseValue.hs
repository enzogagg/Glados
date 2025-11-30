{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-2-glados-1
-- File description:
-- parseValue
-}

module ParseValue (
    parseValue,
    evalValue,
    showValue,
    builtins,
    runExprs
) where

import Types
import System.IO (hFlush, stdout)
import Control.Monad (when)


-- Execute Expr and display results
parseValue :: [Expr] -> IO (Either String ())
parseValue [] = return (Left "empty program")

parseValue code = parseValueRun code builtins


parseValueRun :: [Expr] -> Env -> IO (Either String ())
parseValueRun [] _ = return (Right ())

parseValueRun (x:xs) env = do
    case evalExprForDisplay x env of
        Left err -> return (Left err)
        Right (newEnv, shouldDisplay, val) -> do
            when shouldDisplay $ do
                putStrLn (showValue val)
                hFlush stdout
            parseValueRun xs newEnv

runExprs :: Env -> [Expr] -> IO (Either String Env)
runExprs _c [] =
    return (Left "no expressions to evaluate")

runExprs env (expr:_) = do
    let f oldEnv e =
            case evalExprForDisplay e oldEnv of
                Left err -> Left err
                Right (newEnv, shouldDisplay, val) ->
                    Right (newEnv, shouldDisplay, val)

    case f env expr of
        Left err -> return (Left err)
        Right (newEnv, shouldDisplay, val) -> do
            when shouldDisplay (putStrLn (showValue val))
            return (Right newEnv)

-- Evaluating an Expr for display or not
evalExprForDisplay :: Expr -> Env -> Either String (Env, Bool, Value)
evalExprForDisplay (List [Symbol "define", Symbol name, value]) env = do
    val <- evalValue value env
    return ((name, val) : env, False, Void)

evalExprForDisplay (List (Symbol "define" : List (Symbol fname : params) : body)) env = do
    paramNames <- mapM extractSymbol params
    case body of
        [bodyExpr] ->
            let newEnv = (fname, FuncVal paramNames bodyExpr newEnv) : env
            in return (newEnv, False, Void)
        _ -> Left "function body must be a single expression"

evalExprForDisplay expr env = do
    val <- evalValue expr env
    return (env, True, val)


-- Evaluate an expression and return its value
evalValue :: Expr -> Env -> Either String Value
evalValue (Number n) _ = Right (IntVal n)

evalValue (Boolean b) _ = Right (BoolVal b)

evalValue (Symbol s) env =
    case lookup s env of
        Just val -> Right val
        Nothing -> Left ("unbound symbol: " ++ s)

evalValue (List [Symbol "if", cond, thenExpr, elseExpr]) env = do
    condVal <- evalValue cond env
    case condVal of
        BoolVal True -> evalValue thenExpr env
        BoolVal False -> evalValue elseExpr env
        _ -> Left "if condition must be a boolean"

evalValue (List [Symbol "lambda", List params, body]) env = do
    paramNames <- mapM extractSymbol params
    return (FuncVal paramNames body env)

evalValue (List (func : args)) env = do
    funcVal <- evalValue func env
    argVals <- mapM (`evalValue` env) args
    applyFunc funcVal argVals env

evalValue (List []) _ = Left "empty list is not a valid expression"


-- Apply a function to its arguments
applyFunc :: Value -> [Value] -> Env -> Either String Value
applyFunc (Primitive f) args _ = f args

applyFunc (FuncVal params body closureEnv) args _ = do
    if length params /= length args
        then Left $ "function expects " ++ show (length params) ++
                    " arguments, got " ++ show (length args)
    else do
        let newEnv = zip params args ++ closureEnv
        evalValue body newEnv

applyFunc _ _ _ = Left "attempt to call a non-function value"


extractSymbol :: Expr -> Either String String
extractSymbol (Symbol s) = Right s

extractSymbol _ = Left "expected symbol"


-- Builtin functions
builtins :: Env
builtins =
    [ ("+", Primitive primAdd)
    , ("-", Primitive primSub)
    , ("*", Primitive primMul)
    , ("div", Primitive primDiv)
    , ("mod", Primitive primMod)
    , ("<", Primitive primLt)
    , ("eq?", Primitive primEq)
    ]


primAdd :: [Value] -> Either String Value
primAdd [IntVal a, IntVal b] = Right (IntVal (a + b))

primAdd _ = Left "+ requires two integer arguments"


primSub :: [Value] -> Either String Value
primSub [IntVal a, IntVal b] = Right (IntVal (a - b))

primSub _ = Left "- requires two integer arguments"


primMul :: [Value] -> Either String Value
primMul [IntVal a, IntVal b] = Right (IntVal (a * b))

primMul _ = Left "* requires two integer arguments"


primDiv :: [Value] -> Either String Value
primDiv [IntVal _, IntVal 0] = Left "division by zero"

primDiv [IntVal a, IntVal b] = Right (IntVal (a `div` b))

primDiv _ = Left "div requires two integer arguments"


primMod :: [Value] -> Either String Value
primMod [IntVal _, IntVal 0] = Left "modulo by zero"

primMod [IntVal a, IntVal b] = Right (IntVal (a `mod` b))

primMod _ = Left "mod requires two integer arguments"


primLt :: [Value] -> Either String Value
primLt [IntVal a, IntVal b] = Right (BoolVal (a < b))

primLt _ = Left "< requires two integer arguments"


primEq :: [Value] -> Either String Value
primEq [IntVal a, IntVal b] = Right (BoolVal (a == b))

primEq [BoolVal a, BoolVal b] = Right (BoolVal (a == b))

primEq _ = Left "eq? requires two arguments of the same type"


showValue :: Value -> String
showValue (IntVal n) = show n

showValue (BoolVal True) = "#t"

showValue (BoolVal False) = "#f"

showValue FuncVal {} = "#<procedure>"

showValue (Primitive _) = "#<primitive-procedure>"

showValue Void = "#<void>"