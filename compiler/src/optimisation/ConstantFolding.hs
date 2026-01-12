{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- ConstantFolding
-}

module ConstantFolding (foldConstants) where

import Types

foldConstants :: AST -> AST
foldConstants = foldNode

foldNode :: AST -> AST
foldNode (IAProgram instrs) = IAProgram (map foldNode instrs)
foldNode (IAFunctionDef name params ret body) = IAFunctionDef name params ret (map foldNode body)
foldNode (IAMain args body) = IAMain args (map foldNode body)
foldNode (IABlock instrs) = IABlock (map foldNode instrs)

foldNode (IAInfix left op right) = simplifyInfix op (foldNode left) (foldNode right)
foldNode (IAIf cond thenB elseB) = IAIf (foldNode cond) (map foldNode thenB) (fmap (map foldNode) elseB)
foldNode (IAWhile cond body) = IAWhile (foldNode cond) (map foldNode body)
foldNode (IAFor initE cond inc body) = IAFor (foldNode initE) (foldNode cond) (foldNode inc) (map foldNode body)
foldNode (IAReturn expr) = IAReturn (foldNode expr)
foldNode (IADeclare name typ expr) = IADeclare name typ (foldNode expr)
foldNode (IAAssign name expr) = IAAssign name (foldNode expr)
foldNode (IACall name args) = IACall name (map foldNode args)
foldNode (IATuple exprs) = IATuple (map foldNode exprs)
foldNode (IAList exprs) = IAList (map foldNode exprs)
foldNode node = node

simplifyInfix :: String -> AST -> AST -> AST
simplifyInfix "+" (IANumber a) (IANumber b) = IANumber (a + b)
simplifyInfix "-" (IANumber a) (IANumber b) = IANumber (a - b)
simplifyInfix "*" (IANumber a) (IANumber b) = IANumber (a * b)
simplifyInfix "mod" (IANumber a) (IANumber b)
    | b /= 0    = IANumber (a `mod` b)
    | otherwise = IAInfix (IANumber a) "mod" (IANumber b)
simplifyInfix "div" (IANumber a) (IANumber b)
    | b /= 0    = IANumber (a `div` b)
    | otherwise = IAInfix (IANumber a) "div" (IANumber b)

simplifyInfix "==" (IANumber a) (IANumber b) = IABoolean (a == b)
simplifyInfix "!=" (IANumber a) (IANumber b) = IABoolean (a /= b)
simplifyInfix "<"  (IANumber a) (IANumber b) = IABoolean (a < b)
simplifyInfix ">"  (IANumber a) (IANumber b) = IABoolean (a > b)
simplifyInfix "<=" (IANumber a) (IANumber b) = IABoolean (a <= b)
simplifyInfix ">=" (IANumber a) (IANumber b) = IABoolean (a >= b)

simplifyInfix "et" (IABoolean a) (IABoolean b) = IABoolean (a && b)
simplifyInfix "ou" (IABoolean a) (IABoolean b) = IABoolean (a || b)

simplifyInfix "+" (IAString a) (IAString b) = IAString (a ++ b)

simplifyInfix "+" (IAFloatLiteral a) (IAFloatLiteral b) = IAFloatLiteral (a + b)
simplifyInfix "-" (IAFloatLiteral a) (IAFloatLiteral b) = IAFloatLiteral (a - b)
simplifyInfix "*" (IAFloatLiteral a) (IAFloatLiteral b) = IAFloatLiteral (a * b)
simplifyInfix "/" (IAFloatLiteral a) (IAFloatLiteral b)
    | b /= 0    = IAFloatLiteral (a / b)
    | otherwise = IAInfix (IAFloatLiteral a) "/" (IAFloatLiteral b)

simplifyInfix op l r = IAInfix l op r
