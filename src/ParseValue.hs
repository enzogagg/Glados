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
    runExprs,
    primAdd,
    primSub,
    primMul,
    primDiv,
    primMod,
    primLt,
    primEq,
    primCons,
    primCar,
    primCdr,
    primList,
    primNull
) where

import Types
import TailCallOptimization (Bounce(..), runTrampoline)
import System.IO (hFlush, stdout)
import Control.Monad (when)



-- Execute Expr and display results
parseValue :: [Expr] -> IO (Either String ())
parseValue [] = return (Left "empty program")

parseValue code = parseValueRun code builtins


parseValueRun :: [Expr] -> Env -> IO (Either String ())
parseValueRun [] _ = return (Right ())

parseValueRun (x:xs) env = do
    res <- evalExprForDisplay x env
    case res of
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
    let f oldEnv e = do
            res <- evalExprForDisplay e oldEnv
            case res of
                Left err -> return (Left err)
                Right (newEnv, shouldDisplay, val) ->
                    return (Right (newEnv, shouldDisplay, val))

    res <- f env expr
    case res of
        Left err -> return (Left err)
        Right (newEnv, shouldDisplay, val) -> do
            when shouldDisplay (putStrLn (showValue val))
            return (Right newEnv)

-- Evaluating an Expr for display or not
evalExprForDisplay :: Expr -> Env -> IO (Either String (Env, Bool, Value))
evalExprForDisplay (List [Symbol "define", Symbol name, value]) env = do
    bounce <- evalValue value env
    res <- runTrampoline bounce
    case res of
        Right val -> return (Right ((name, val) : env, False, Void))
        Left err -> return (Left err)

evalExprForDisplay (List (Symbol "define" : List (Symbol fname : params) : body)) env = do
    case mapM extractSymbol params of
        Right paramNames ->
            case body of
                [bodyExpr] ->
                    let newEnv = (fname, FuncVal paramNames bodyExpr newEnv) : env
                    in return (Right (newEnv, False, Void))
                _ -> return (Left "function body must be a single expression")
        Left err -> return (Left err)

evalExprForDisplay expr env = do
    bounce <- evalValue expr env
    res <- runTrampoline bounce
    case res of
        Right val -> return (Right (env, True, val))
        Left err -> return (Left err)


-- Evaluate an expression and return its value
evalValue :: Expr -> Env -> IO Bounce
evalValue (Number n) _ = return (Done (Right (IntVal n)))

evalValue (FloatLiteral n) _ = return (Done (Right (FloatVal n)))

evalValue (Boolean b) _ = return (Done (Right (BoolVal b)))

evalValue (String s) _ = return (Done (Right (StringVal s)))

evalValue (Symbol s) env =
    case lookup s env of
        Just val -> return (Done (Right val))
        Nothing -> return (Done (Left ("unbound symbol: " ++ s)))

evalValue (List [Symbol "if", cond, thenExpr, elseExpr]) env = do
    condBounce <- evalValue cond env
    condRes <- runTrampoline condBounce
    case condRes of
        Right (BoolVal True) -> return (Call (\_ -> evalValue thenExpr env) env)
        Right (BoolVal False) -> return (Call (\_ -> evalValue elseExpr env) env)
        Right _ -> return (Done (Left "if condition must be a boolean"))
        Left err -> return (Done (Left err))

evalValue (List [Symbol "lambda", List params, body]) env = do
    case mapM extractSymbol params of
        Right names -> return (Done (Right (FuncVal names body env)))
        Left err -> return (Done (Left err))

evalValue (List [Symbol "quote", expr]) _ = return (Done (Right (exprToValue expr)))

evalValue (List (func : args)) env = do
    funcBounce <- evalValue func env
    funcRes <- runTrampoline funcBounce
    case funcRes of
        Right funcVal -> do
            argVals <- mapM (\arg -> evalValue arg env >>= runTrampoline) args
            -- Check for errors in arguments
            let (errors, vals) = foldr (\res (es, vs) -> case res of
                                            Left e -> (e:es, vs)
                                            Right v -> (es, v:vs)) ([], []) argVals
            case errors of
                [] -> applyFunc funcVal vals env
                (err:_) -> return (Done (Left err))
        Left err -> return (Done (Left err))

evalValue (List []) _ = return (Done (Left "empty list is not a valid expression"))

exprToValue :: Expr -> Value
exprToValue (Number n) = IntVal n
exprToValue (FloatLiteral n) = FloatVal n
exprToValue (Boolean b) = BoolVal b
exprToValue (Symbol s) = SymbolVal s
exprToValue (String s) = StringVal s
exprToValue (List xs) = ListVal (map exprToValue xs)


-- Apply a function to its arguments
applyFunc :: Value -> [Value] -> Env -> IO Bounce
applyFunc (Primitive f) args _ = return (Done (f args))

applyFunc (FuncVal params body closureEnv) args _ = do
    if length params /= length args
        then return (Done (Left $ "function expects " ++ show (length params) ++
                    " arguments, got " ++ show (length args)))
    else do
        let newEnv = zip params args ++ closureEnv
        return (Call (\_ -> evalValue body newEnv) newEnv)

applyFunc _ _ _ = return (Done (Left "attempt to call a non-function value"))


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
    , ("cons", Primitive primCons)
    , ("car", Primitive primCar)
    , ("cdr", Primitive primCdr)
    , ("list", Primitive primList)
    , ("null?", Primitive primNull)
    ]


primAdd :: [Value] -> Either String Value
primAdd [IntVal a, IntVal b] = Right (IntVal (a + b))

primAdd [FloatVal a, FloatVal b] = Right (FloatVal (a + b))

primAdd _ = Left "+ requires two integer arguments"


primSub :: [Value] -> Either String Value
primSub [IntVal a, IntVal b] = Right (IntVal (a - b))

primSub [FloatVal a, FloatVal b] = Right (FloatVal (a - b))

primSub _ = Left "- requires two integer arguments"


primMul :: [Value] -> Either String Value
primMul [IntVal a, IntVal b] = Right (IntVal (a * b))

primMul [FloatVal a, FloatVal b] = Right (FloatVal (a * b))

primMul _ = Left "* requires two integer arguments"


primDiv :: [Value] -> Either String Value
primDiv [IntVal _, IntVal 0] = Left "division by zero"

primDiv [FloatVal _, FloatVal 0] = Left "division by zero"

primDiv [IntVal a, IntVal b] = Right (IntVal (a `div` b))

primDiv [FloatVal a, FloatVal b] = Right (FloatVal (a / b))

primDiv _ = Left "div requires two integer arguments"


primMod :: [Value] -> Either String Value
primMod [IntVal _, IntVal 0] = Left "modulo by zero"

primMod [FloatVal _, FloatVal 0] = Left "modulo by zero"

primMod [IntVal a, IntVal b] = Right (IntVal (a `mod` b))

primMod _ = Left "mod requires two integer arguments"




primLt :: [Value] -> Either String Value
primLt [IntVal a, IntVal b] = Right (BoolVal (a < b))

primLt [FloatVal a, FloatVal b] = Right (BoolVal (a < b))

primLt _ = Left "< requires two integer arguments"


primEq :: [Value] -> Either String Value
primEq [IntVal a, IntVal b] = Right (BoolVal (a == b))

primEq [FloatVal a, FloatVal b] = Right (BoolVal (a == b))

primEq [BoolVal a, BoolVal b] = Right (BoolVal (a == b))

primEq _ = Left "eq? requires two arguments of the same type"


showValue :: Value -> String
showValue (IntVal n) = show n

showValue (FloatVal n) = show n

showValue (BoolVal True) = "#t"

showValue (BoolVal False) = "#f"

showValue FuncVal {} = "#<procedure>"

showValue (Primitive _) = "#<primitive-procedure>"

showValue (ListVal xs) = "(" ++ unwords (map show xs) ++ ")"

showValue (SymbolVal s) = s

showValue (StringVal s) = s

showValue Void = "#<void>"

primCons :: [Value] -> Either String Value
primCons [x, ListVal xs] = Right (ListVal (x : xs))

primCons [_, _] = Left "cons requires a list as the second argument"

primCons _ = Left "cons requires two arguments"

primCar :: [Value] -> Either String Value
primCar [ListVal (x:_)] = Right x

primCar [ListVal []] = Left "car of empty list"

primCar [_] = Left "car requires a list"

primCar _ = Left "car requires one argument"

primCdr :: [Value] -> Either String Value
primCdr [ListVal (_:xs)] = Right (ListVal xs)

primCdr [ListVal []] = Left "cdr of empty list"

primCdr [_] = Left "cdr requires a list"

primCdr _ = Left "cdr requires one argument"

primList :: [Value] -> Either String Value
primList xs = Right (ListVal xs)

primNull :: [Value] -> Either String Value
primNull [ListVal []] = Right (BoolVal True)
primNull [ListVal _] = Right (BoolVal False)
primNull [_] = Left "null? requires a list"
primNull _ = Left "null? requires one argument"
