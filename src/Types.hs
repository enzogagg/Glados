{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Types
-}

module Types (
    Expr(..),
    Value(..),
    Env) where

data Expr
    = Number Integer
    | Boolean Bool
    | Symbol String
    | List [Expr]
    deriving (Show, Eq)

data Value
    = IntVal Integer
    | BoolVal Bool
    | FuncVal [String] Expr Env
    | Primitive ([Value] -> Either String Value)
    | Void

-- Instance Show personnalisée
instance Show Value where
    show (IntVal n) = show n
    show (BoolVal True) = "#t"
    show (BoolVal False) = "#f"
    show FuncVal {} = "#<procedure>"
    show (Primitive _) = "#<primitive-procedure>"
    show Void = "#<void>"

-- Instance Eq personnalisée
instance Eq Value where
    IntVal a == IntVal b = a == b
    BoolVal a == BoolVal b = a == b
    FuncVal {} == FuncVal {} = True     -- approximation : toutes les fonctions sont égales
    Primitive _ == Primitive _ = True         -- approximation : tous les primitives sont égales
    Void == Void = True
    _ == _ = False

type Env = [(String, Value)] -- simple environment as an association list
