{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Types
-}

module Types (
    Expr(..),
    AST(..),
    Value(..),
    Env,
    Operator(..),
    CladType(..)) where

import Data.List (intercalate)

-- ==========================
-- Nouveaux Types CLaD
-- ==========================

data CladType
    = IntT
    | FloatT
    | BoolT
    | StringT
    | ListT CladType                -- Liste d'un type spécifique
    | FuncT [CladType] CladType     -- (Args types) -> Return type
    | TupleT [CladType]
    | AnyT
    | VoidT
    | ErrorT
    deriving (Show, Eq)

data Operator
    = Add | Sub | Mul | Div | Mod       -- Arithmétiques
    | Eq | Lt | Gt | Lte | Gte | Neq    -- Comparaison
    | And | Or                          -- Logiques
    deriving (Show, Eq)

-- ==========================
-- Structure LISP conservée (à titre de référence ou pour la première passe)
-- ==========================

data Expr
    = Number Integer
    | FloatLiteral Double
    | Boolean Bool
    | Symbol String
    | String String
    | List [Expr]
    deriving (Show, Eq)

-- ==========================
-- Abstract Syntax Tree (AST) de CLaD
-- ==========================

data AST
    -- Expressions de base
    = IANumber Integer
    | IAFloatLiteral Double
    | IABoolean Bool
    | IAString String
    | IASymbol String                           -- Identifiant de variable ou de fonction
    | IAList [AST]                              -- Construction d'une liste (ex: [1, 2, 3] si implémenté)
    | IAUnit                                    -- Représente le Neant (:unit ou void)
    | IATuple [AST]                             -- Représente les Tuples (sous forme Variable (element, element))

    -- Expressions Infixées (Opérateurs Builtin ou Utilisateur)
    | IAInfix AST String AST                    -- Left Symbole/Opérateur Right (ex: 1 "+" 2)

    -- Statements / Instructions (Blocs Impératifs)
    | IAProgram [AST]                           -- Le programme complet (Déclarations + Fonctions + Main)
    | IAMain [AST]                              -- Le corps du bloc 'principal' (liste d'instructions)
    | IABlock [AST]                             -- Utilisé pour les blocs locaux

    -- Déclarations & Assignations
    | IADeclare String (Maybe CladType) AST     -- constante/variable Nom (Type) = Value
    | IAAssign String AST                       -- Ré-assignation (x = value)

    -- Structures de Contrôle
    | IAIf AST [AST] (Maybe [AST])              -- si cond then {body} else {body}
    | IAWhile AST [AST]                         -- tantque cond {body}
    | IAFor AST AST AST [AST]                   -- pour i de start à end {body}
    | IAReturn AST                              -- retourner expression

    -- Fonctions
    | IAFunctionDef String [(String, Maybe CladType)] (Maybe CladType) [AST]
                                                -- Nom, [(ArgName, Type)], (ReturnType), Corps (Statements)
    | IACall String [AST]                       -- Appel de fonction: func(args)
    deriving (Show, Eq)

-- ==========================
-- Runtime Values
-- ==========================

data Value
    = IntVal Integer
    | FloatVal Double
    | BoolVal Bool
    | FuncVal [String] [AST] Env -- CHANGÉ : Le corps est une liste de statements [AST]
    | Primitive ([Value] -> Either String Value)
    | ListVal [Value]
    | SymbolVal String
    | StringVal String
    | TupleVal [Value]
    | Void                      -- Représente le type Neant
    | ErrorVal String           -- Ajout d'un type pour les erreurs (plus propre que Left String)

-- Instance Show personnalisée
instance Show Value where
    show (IntVal n) = show n
    show (FloatVal n) = show n
    show (BoolVal True) = "#t"
    show (BoolVal False) = "#f"
    show FuncVal {} = "#<procedure>"
    show (Primitive _) = "#<primitive-procedure>"
    show (ListVal list) = "(" ++ unwords (map show list) ++ ")"
    show (TupleVal t) = "(" ++ intercalate ", " (map show t) ++ ")"
    show (SymbolVal s) = s
    show (StringVal s) = s
    show Void = "#<void>"
    show (ErrorVal s) = "*** ERROR: " ++ s -- Affichage d'erreur

-- Instance Eq personnalisée
instance Eq Value where
    IntVal a == IntVal b = a == b
    FloatVal a == FloatVal b = a == b
    BoolVal a == BoolVal b = a == b
    FuncVal {} == FuncVal {} = True
    Primitive _ == Primitive _ = True
    ListVal a == ListVal b = a == b
    SymbolVal a == SymbolVal b = a == b
    StringVal a == StringVal b = a == b
    Void == Void = True
    ErrorVal a == ErrorVal b = a == b -- Les erreurs sont égales si leurs messages sont égaux (ou si on considère que toutes les erreurs sont égales)
    _ == _ = False

type Env = [(String, Value)] -- simple environment as an association list