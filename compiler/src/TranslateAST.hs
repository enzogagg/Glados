{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- TranslateAST
-- Handles the conversion from raw parsed Expr to validated IAST
-}

module TranslateAST (
    translateExpr
) where

import Types

-- =========================
-- Translation (Expr -> IAST)
-- =========================

extractSymbol :: Expr -> Either String String
extractSymbol (Symbol s) = Right s
extractSymbol _ = Left "expected symbol"

exprToIAST :: Expr -> IAST
exprToIAST (Number n) = IANumber n
exprToIAST (FloatLiteral n) = IAFloatLiteral n
exprToIAST (Boolean b) = IABoolean b
exprToIAST (Symbol s) = IASymbol s
exprToIAST (String s) = IAString s
exprToIAST (List xs) = IAList (map exprToIAST xs)

translateExpr :: Expr -> Either String IAST
translateExpr (Number n) = Right (IANumber n)
translateExpr (FloatLiteral n) = Right (IAFloatLiteral n)
translateExpr (Boolean b) = Right (IABoolean b)
translateExpr (Symbol s) = Right (IASymbol s)
translateExpr (String s) = Right (IAString s)

translateExpr (List [Symbol "if", cond, thenExpr, elseExpr]) = do
    condIAST <- translateExpr cond
    thenIAST <- translateExpr thenExpr
    elseIAST <- translateExpr elseExpr
    return (IAIf condIAST thenIAST elseIAST)

translateExpr (List [Symbol "lambda", List params, body]) = do
    paramNames <- mapM extractSymbol params
    bodyIAST <- translateExpr body
    return (IALambda paramNames bodyIAST)

translateExpr (List [Symbol "define", Symbol name, value]) = do
    valueIAST <- translateExpr value
    return (IADefine name valueIAST)

translateExpr (List [Symbol "quote", expr]) = do
    let quotedIAST = exprToIAST expr
    return (IAQuote quotedIAST)

translateExpr (List (Symbol "define" : List (Symbol fname : params) : body)) = do
    paramNames <- mapM extractSymbol params
    case body of
        [bodyExpr] -> do
            bodyIAST <- translateExpr bodyExpr
            let lambdaIAST = IALambda paramNames bodyIAST
            return (IADefine fname lambdaIAST)

        _ -> Left "Function body must be a single expression"

translateExpr (List xs) = do
    iasts <- mapM translateExpr xs
    return (IAList iasts)

translateExpr (List (Symbol s : _))
    | s `elem` ["if", "lambda", "define", "quote"] = Left $ "Invalid number of arguments for special form: " ++ s
translateExpr (List []) = Left "empty list is not a valid expression"
translateExpr _ = Left "Translation error: Unrecognized expression structure."
