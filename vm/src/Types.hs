module Types where

data Value
    = IntVal Int              -- Tag 00
    | FloatVal Float          -- Tag 01
    | BoolVal Bool            -- Tag 02
    | CharVal Char            -- Tag 03
    | StringVal String        -- Tag 04
    | ListVal [Value]         -- Tag 05
    | SymbolVal String        -- Tag 06
    | NilVal                  -- Tag 07
    | FunctionVal Int Int     -- Tag 08
    deriving (Show, Eq)

data Instruction
    = PushConst Int           -- 01
    | PushInt Int             -- 02
    | PushFloat Float         -- 03
    | PushBool Bool           -- 04
    | PushString String       -- 05
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

    | Load String             -- 50
    | Store String            -- 51
    | Define String           -- 52

    | Jump Int                -- 60
    | JumpIfTrue Int          -- 61
    | JumpIfFalse Int         -- 62
    | Call Int Int            -- 70
    | Return                  -- 71

    | Print                   -- 80
    | Input                   -- 81

    | Halt                    -- FF
    deriving (Show, Eq)

data Header = Header {
    magic :: Int,
    version :: Int,
    flags :: Int
} deriving (Show, Eq)

data FunctionMeta = FunctionMeta {
    funcId :: Int,
    funcAddress :: Int,
    funcArgCount :: Int
} deriving (Show, Eq)

data BytecodeFile = BytecodeFile {
    header :: Header,
    constants :: [Value],
    functions :: [FunctionMeta],
    instructions :: [Instruction]
} deriving (Show, Eq)
