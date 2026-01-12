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
    CladType(..),
    Opcode(..),
    TypeTag(..),
    ConstantEntry(..),
    FunctionEntry(..),
    opcodeToByte,
    typeTagToByte) where

import Data.List (intercalate)
import Data.Word
import qualified Data.Map as Map

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
    | IAMain [String] [AST]                     -- Le corps du bloc 'principal' (arguments) (liste d'instructions)
    | IABlock [AST]                             -- Utilisé pour les blocs locaux

    -- Déclarations & Assignations
    | IAInclude String                          -- inclure "file.clad"
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
    show (BoolVal True) = "vrai"
    show (BoolVal False) = "faux"
    show FuncVal {} = "#<procedure>"
    show (Primitive _) = "#<primitive-procedure>"
    show (ListVal list) = "(" ++ unwords (map show list) ++ ")"
    show (TupleVal t) = "(" ++ intercalate ", " (map show t) ++ ")"
    show (SymbolVal s) = s
    show (StringVal s) = s
    show Void = "neant"
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

type Env = Map.Map String Value

-- ==========================
-- Types pour le Bytecode
-- ==========================

data ConstantEntry
    = ConstInt Integer
    | ConstFloat Double
    | ConstBool Bool
    | ConstString String
    | ConstSymbol String
    deriving (Show, Eq)

data FunctionEntry = FunctionEntry
    { funcIndex :: Int
    , funcAddress :: Int
    , funcArgCount :: Int
    } deriving (Show)

data TypeTag
    = TagInt        -- 0x00
    | TagFloat      -- 0x01
    | TagBool       -- 0x02
    | TagChar       -- 0x03
    | TagString     -- 0x04
    | TagList       -- 0x05
    | TagSymbol     -- 0x06
    | TagNil        -- 0x07
    | TagFunction   -- 0x08
    deriving (Show, Eq)

typeTagToByte :: TypeTag -> Word8
typeTagToByte TagInt = 0x00
typeTagToByte TagFloat = 0x01
typeTagToByte TagBool = 0x02
typeTagToByte TagChar = 0x03
typeTagToByte TagString = 0x04
typeTagToByte TagList = 0x05
typeTagToByte TagSymbol = 0x06
typeTagToByte TagNil = 0x07
typeTagToByte TagFunction = 0x08

data Opcode
    = OpPushConst       -- 0x01
    | OpPushInt         -- 0x02
    | OpPushFloat       -- 0x03
    | OpPushBool        -- 0x04
    | OpPushString      -- 0x05
    | OpPushNil         -- 0x06
    | OpPop             -- 0x07
    | OpAdd             -- 0x10
    | OpSub             -- 0x11
    | OpMul             -- 0x12
    | OpDiv             -- 0x13
    | OpMod             -- 0x14
    | OpNeg             -- 0x15
    | OpEq              -- 0x20
    | OpNeq             -- 0x21
    | OpLt              -- 0x22
    | OpGt              -- 0x23
    | OpLte             -- 0x24
    | OpGte             -- 0x25
    | OpAnd             -- 0x26
    | OpOr              -- 0x27
    | OpNot             -- 0x28
    | OpCons            -- 0x30
    | OpHead            -- 0x31
    | OpTail            -- 0x32
    | OpList            -- 0x33
    | OpLen             -- 0x34
    | OpMakeSymbol      -- 0x40
    | OpQuote           -- 0x41
    | OpEval            -- 0x42
    | OpLoad            -- 0x50
    | OpStore           -- 0x51
    | OpDefine          -- 0x52
    | OpJmp             -- 0x60
    | OpJmpIfTrue       -- 0x61
    | OpJmpIfFalse      -- 0x62
    | OpCall            -- 0x70
    | OpReturn          -- 0x71
    | OpClosure         -- 0x72
    | OpLoadArg         -- 0x73
    | OpPrint           -- 0x80
    | OpInput           -- 0x81
    | OpHalt            -- 0xFF
    deriving (Show, Eq)

opcodeToByte :: Opcode -> Word8
opcodeToByte OpPushConst = 0x01
opcodeToByte OpPushInt = 0x02
opcodeToByte OpPushFloat = 0x03
opcodeToByte OpPushBool = 0x04
opcodeToByte OpPushString = 0x05
opcodeToByte OpPushNil = 0x06
opcodeToByte OpPop = 0x07
opcodeToByte OpAdd = 0x10
opcodeToByte OpSub = 0x11
opcodeToByte OpMul = 0x12
opcodeToByte OpDiv = 0x13
opcodeToByte OpMod = 0x14
opcodeToByte OpNeg = 0x15
opcodeToByte OpEq = 0x20
opcodeToByte OpNeq = 0x21
opcodeToByte OpLt = 0x22
opcodeToByte OpGt = 0x23
opcodeToByte OpLte = 0x24
opcodeToByte OpGte = 0x25
opcodeToByte OpAnd = 0x26
opcodeToByte OpOr = 0x27
opcodeToByte OpNot = 0x28
opcodeToByte OpCons = 0x30
opcodeToByte OpHead = 0x31
opcodeToByte OpTail = 0x32
opcodeToByte OpList = 0x33
opcodeToByte OpLen = 0x34
opcodeToByte OpMakeSymbol = 0x40
opcodeToByte OpQuote = 0x41
opcodeToByte OpEval = 0x42
opcodeToByte OpLoad = 0x50
opcodeToByte OpStore = 0x51
opcodeToByte OpDefine = 0x52
opcodeToByte OpJmp = 0x60
opcodeToByte OpJmpIfTrue = 0x61
opcodeToByte OpJmpIfFalse = 0x62
opcodeToByte OpCall = 0x70
opcodeToByte OpReturn = 0x71
opcodeToByte OpClosure = 0x72
opcodeToByte OpLoadArg = 0x73
opcodeToByte OpPrint = 0x80
opcodeToByte OpInput = 0x81
opcodeToByte OpHalt = 0xFF
