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
import TranslateAST (translateExpr)
import TailCallOptimization (Bounce(..), runTrampoline)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import qualified Data.List as L (lookup)

-- =========================
-- Evaluation (IAST -> Value)
-- =========================

createFuncVal :: String -> [String] -> IAST -> Env -> Value
createFuncVal fname params body env = result
  where
    newEnv = (fname, result) : env
    result = FuncVal params body newEnv


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

-- Evaluating an Expr for display or not (Pipeline start: Expr -> IAST -> Value)
evalExprForDisplay :: Expr -> Env -> IO (Either String (Env, Bool, Value))
evalExprForDisplay expr env =
    case translateExpr expr of
        Left err -> return (Left err)
        Right iast -> do
            case iast of
                IADefine name (IALambda params body) ->
                    let funcVal = createFuncVal name params body env
                    in return (Right ((name, funcVal) : env, False, Void))

                _ -> do
                    bounce <- evalValue iast env
                    res <- runTrampoline bounce
                    case res of
                        Right val ->
                            case iast of
                                IADefine name valIAST ->
                                    return (Right ((name, val) : env, False, Void))
                                _ ->
                                    return (Right (env, True, val))
                        Left err -> return (Left err)


-- Evaluate an expression and return its value
evalValue :: IAST -> Env -> IO Bounce
evalValue (IANumber n) _ = return (Done (Right (IntVal n)))

evalValue (IAFloatLiteral n) _ = return (Done (Right (FloatVal n)))

evalValue (IABoolean b) _ = return (Done (Right (BoolVal b)))

evalValue (IAString s) _ = return (Done (Right (StringVal s)))

evalValue (IASymbol s) env =
    case L.lookup s env of
        Just val -> return (Done (Right val))
        Nothing -> return (Done (Left ("unbound symbol: " ++ s)))

evalValue (IAIf cond thenIAST elseIAST) env = do
    condBounce <- evalValue cond env
    condRes <- runTrampoline condBounce
    case condRes of
        Right (BoolVal True) -> return (Call (\_ -> evalValue thenIAST env) env)
        Right (BoolVal False) -> return (Call (\_ -> evalValue elseIAST env) env)
        Right _ -> return (Done (Left "if condition must be a boolean"))
        Left err -> return (Done (Left err))

evalValue (IALambda params body) env =
    return (Done (Right (FuncVal params body env)))

evalValue (IAQuote expr) _ =
    return (Done (Right (iastToValue expr)))

evalValue (IADefine _ (IALambda _ _)) _ =
    return (Done (Right Void))

evalValue (IADefine _ valueIAST) env = evalValue valueIAST env

-- General Function Call
evalValue (IAList (func : args)) env = do
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

evalValue (IAList []) _ = return (Done (Left "empty list is not a valid function call"))
evalValue _ _ = return (Done (Left "Unrecognized IAST form"))

-- Convert IAST content into a Value for 'quote'
iastToValue :: IAST -> Value
iastToValue (IANumber n) = IntVal n
iastToValue (IAFloatLiteral n) = FloatVal n
iastToValue (IABoolean b) = BoolVal b
iastToValue (IASymbol s) = SymbolVal s
iastToValue (IAString s) = StringVal s
iastToValue (IAList xs) = ListVal (map iastToValue xs)
iastToValue _ = ListVal []

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


-- Builtin functions (unchanged)
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

-- Primitive functions (unchanged)
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