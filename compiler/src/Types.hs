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
    typeTagToByte,
    getOpcodeSize,
    getTypeTagSize) where

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
    | ConstChar Char
    | ConstString String
    | ConstList [ConstantEntry]
    | ConstSymbol String
    | ConstNil
    | ConstTuple [ConstantEntry]
    | ConstArray [ConstantEntry]
    | ConstStruct [(String, ConstantEntry)]
    | ConstMap [(ConstantEntry, ConstantEntry)]
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
    | TagTuple      -- 0x09
    | TagArray      -- 0x0A
    | TagStruct     -- 0x0B
    | TagMap        -- 0x0C
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
typeTagToByte TagTuple = 0x09
typeTagToByte TagArray = 0x0A
typeTagToByte TagStruct = 0x0B
typeTagToByte TagMap = 0x0C

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
    | OpMakeTuple       -- 0x90
    | OpTupleGet        -- 0x91
    | OpMakeArray       -- 0x92
    | OpArrayGet        -- 0x93
    | OpArraySet        -- 0x94
    | OpMakeMap         -- 0x95
    | OpMapGet          -- 0x96
    | OpMapSet          -- 0x97
    | OpMakeStruct      -- 0x98
    | OpStructGet       -- 0x99
    | OpStructSet       -- 0x9A
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
opcodeToByte OpMakeTuple = 0x90
opcodeToByte OpTupleGet = 0x91
opcodeToByte OpMakeArray = 0x92
opcodeToByte OpArrayGet = 0x93
opcodeToByte OpArraySet = 0x94
opcodeToByte OpMakeMap = 0x95
opcodeToByte OpMapGet = 0x96
opcodeToByte OpMapSet = 0x97
opcodeToByte OpMakeStruct = 0x98
opcodeToByte OpStructGet = 0x99
opcodeToByte OpStructSet = 0x9A
opcodeToByte OpHalt = 0xFF

-- ==========================
-- Taille de données par type tag (pour validation)
-- ==========================

getTypeTagSize :: TypeTag -> Int
getTypeTagSize TagInt = 4        -- 4 bytes pour un Int32
getTypeTagSize TagFloat = 4      -- 4 bytes pour un Float32 (IEEE 754)
getTypeTagSize TagBool = 1       -- 1 byte pour un booléen
getTypeTagSize TagChar = 1       -- 1 byte pour un caractère
getTypeTagSize TagString = -1    -- Taille variable
getTypeTagSize TagList = -1      -- Taille variable
getTypeTagSize TagSymbol = -1    -- Taille variable
getTypeTagSize TagNil = 0        -- Pas de données
getTypeTagSize TagFunction = 4   -- 4 bytes pour l'index de fonction
getTypeTagSize TagTuple = -1     -- Taille variable
getTypeTagSize TagArray = -1     -- Taille variable
getTypeTagSize TagStruct = -1    -- Taille variable
getTypeTagSize TagMap = -1       -- Taille variable

-- ==========================
-- Taille des opérandes pour chaque opcode
-- ==========================

getOpcodeSize :: Word8 -> Int
getOpcodeSize op
    | op == 0x01 = 4  -- PUSH_CONST
    | op == 0x02 = 4  -- PUSH_INT
    | op == 0x03 = 4  -- PUSH_FLOAT (Float32 IEEE-754)
    | op == 0x04 = 1  -- PUSH_BOOL
    | op == 0x05 = 4  -- PUSH_STRING
    | op == 0x06 = 0  -- PUSH_NIL
    | op == 0x07 = 0  -- POP
    | op == 0x10 = 0  -- ADD
    | op == 0x11 = 0  -- SUB
    | op == 0x12 = 0  -- MUL
    | op == 0x13 = 0  -- DIV
    | op == 0x14 = 0  -- MOD
    | op == 0x15 = 0  -- NEG
    | op == 0x20 = 0  -- EQ
    | op == 0x21 = 0  -- NEQ
    | op == 0x22 = 0  -- LT
    | op == 0x23 = 0  -- GT
    | op == 0x24 = 0  -- LTE
    | op == 0x25 = 0  -- GTE
    | op == 0x30 = 0  -- AND
    | op == 0x31 = 0  -- OR
    | op == 0x32 = 0  -- NOT
    | op == 0x40 = 4  -- LIST
    | op == 0x41 = 0  -- LIST_GET
    | op == 0x50 = 4  -- LOAD
    | op == 0x51 = 4  -- STORE
    | op == 0x52 = 4  -- DEFINE
    | op == 0x60 = 4  -- JMP
    | op == 0x61 = 4  -- JMP_IF_TRUE
    | op == 0x62 = 4  -- JMP_IF_FALSE
    | op == 0x70 = 5  -- CALL
    | op == 0x71 = 0  -- RETURN
    | op == 0x72 = 4  -- CLOSURE
    | op == 0x73 = 4  -- LOAD_ARG
    | op == 0x80 = 0  -- PRINT
    | op == 0xFF = 0  -- HALT
    | otherwise = 0
