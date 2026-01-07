module Bytecode (
    BytecodeFile(..),
    BCValue(..),
    Instruction(..),
    FunctionMeta(..)
) where

-- Value definition for Bytecode (Constants pool)
-- Mirroring VM types
data BCValue
    = BCInt Int           -- 00
    | BCFloat Float       -- 01
    | BCBool Bool         -- 02
    | BCChar Char         -- 03
    | BCString String     -- 04
    | BCList [BCValue]    -- 05
    | BCSymbol String     -- 06
    | BCNil               -- 07
    | BCFunction Int      -- 08
    deriving (Show, Eq)

data Instruction
    = PushConst Int           -- 01
    | PushInt Int             -- 02
    | PushFloat Float         -- 03
    | PushBool Bool           -- 04
    | PushString Int          -- 05 (Int index to pool)
    | PushNil                 -- 06
    | Pop                     -- 07

    | Add                     -- 10
    | Sub                     -- 11
    | Mul                     -- 12
    | Div                     -- 13
    | Mod                     -- 14
    | Neg                     -- 15

    | Eq                      -- 20
    | Neq                     -- 21
    | Lt                      -- 22
    | Gt                      -- 23
    | Le                      -- 24
    | Ge                      -- 25

    | Cons                    -- 30
    | Head                    -- 31
    | Tail                    -- 32
    | ListMake Int            -- 33
    | Len                     -- 34

    -- Variables
    | Load Int                -- 50 (Int index to string pool for var name)
    | Store Int               -- 51
    | Define Int              -- 52

    -- Flow
    | Jump Int                -- 60
    | JumpIfTrue Int          -- 61
    | JumpIfFalse Int         -- 62

    -- Functions
    | Call Int Int            -- 70 (FuncIndex, ArgCount)
    | Return                  -- 71
    | Closure Int             -- 72
    | LoadArg Int             -- 73

    -- IO
    | Print                   -- 80
    | Input                   -- 81

    -- File IO
    | OpenFile                -- A0
    | ReadFile                -- A1
    | WriteFile               -- A2
    | CloseFile               -- A3

    | Halt                    -- FF
    deriving (Show, Eq)

data FunctionMeta = FunctionMeta {
    funcId :: Int,
    funcAddress :: Int,
    funcArgCount :: Int
} deriving (Show, Eq)

data BytecodeFile = BytecodeFile {
    magic :: Int,
    version :: Int,
    flags :: Int,
    constants :: [BCValue],
    functions :: [FunctionMeta],
    instructions :: [Instruction]
} deriving (Show, Eq)
