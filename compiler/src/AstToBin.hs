{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-2-glados-1
-- File description:
-- AstToBin
-}

module AstToBin (
    parseBin,
) where

import Types
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Control.Monad.State
import qualified Data.Map.Strict as Map

-- ==========================
-- Types pour la génération
-- ==========================

data BytecodeContext = BytecodeContext
    { constPool :: [ConstantEntry]
    , constMap :: Map.Map String Int
    , funcTable :: [FunctionEntry]
    , funcMap :: Map.Map String Int
    , nextConstIndex :: Int
    , nextFuncIndex :: Int
    } deriving (Show)

emptyContext :: BytecodeContext
emptyContext = BytecodeContext [] Map.empty [] Map.empty 0 0

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

-- ==========================
-- Génération du Header
-- ==========================

generateHeader :: Put
generateHeader = do
    -- Magic number "CBC\0"
    putWord8 0x43
    putWord8 0x42
    putWord8 0x43
    putWord8 0x00
    -- Version 1.0
    putWord8 0x01
    putWord8 0x00
    -- Flags
    putWord8 0x00
    -- Reserved (3 bytes)
    putWord8 0x00
    putWord8 0x00
    putWord8 0x00

-- ==========================
-- Génération du Constant Pool
-- ==========================

putConstantEntry :: ConstantEntry -> Put
putConstantEntry (ConstInt n) = do
    putWord8 (typeTagToByte TagInt)
    putWord32be 4
    putWord32be (fromIntegral n)

putConstantEntry (ConstFloat f) = do
    putWord8 (typeTagToByte TagFloat)
    putWord32be 8
    putDoublebe f

putConstantEntry (ConstBool b) = do
    putWord8 (typeTagToByte TagBool)
    putWord32be 1
    putWord8 (if b then 0x01 else 0x00)

putConstantEntry (ConstString s) = do
    putWord8 (typeTagToByte TagString)
    let bytes = map (fromIntegral . fromEnum) s
    putWord32be (fromIntegral $ length bytes)
    mapM_ putWord8 bytes

putConstantEntry (ConstSymbol s) = do
    putWord8 (typeTagToByte TagSymbol)
    let bytes = map (fromIntegral . fromEnum) s
    putWord32be (fromIntegral $ length bytes)
    mapM_ putWord8 bytes

generateConstantPool :: [ConstantEntry] -> Put
generateConstantPool entries = do
    putWord32be (fromIntegral $ length entries)
    mapM_ putConstantEntry entries

-- ==========================
-- Génération de la Function Table
-- ==========================

putFunctionEntry :: FunctionEntry -> Put
putFunctionEntry fe = do
    putWord32be (fromIntegral $ funcIndex fe)
    putWord32be (fromIntegral $ funcAddress fe)
    putWord8 (fromIntegral $ funcArgCount fe)

generateFunctionTable :: [FunctionEntry] -> Put
generateFunctionTable entries = do
    putWord32be (fromIntegral $ length entries)
    mapM_ putFunctionEntry entries

-- ==========================
-- Génération des instructions
-- ==========================

type CodeGen = State BytecodeContext (Either String [Word8])

addConstant :: ConstantEntry -> State BytecodeContext Int
addConstant entry = do
    ctx <- get
    let key = show entry
    case Map.lookup key (constMap ctx) of
        Just idx -> return idx
        Nothing -> do
            let idx = nextConstIndex ctx
            put ctx { constPool = constPool ctx ++ [entry]
                    , constMap = Map.insert key idx (constMap ctx)
                    , nextConstIndex = idx + 1
                    }
            return idx

generateInstruction :: AST -> CodeGen
generateInstruction (IANumber n) = do
    if n >= -2147483648 && n <= 2147483647
        then do
            let bytes = BL.unpack $ runPut $ putWord32be (fromIntegral n)
            return $ Right $ opcodeToByte OpPushInt : bytes
        else return $ Left $ "Integer value out of range (must fit in 4 bytes): " ++ show n

generateInstruction (IAFloatLiteral f) = do
    let bytes = BL.unpack $ runPut $ putDoublebe f
    return $ Right $ opcodeToByte OpPushFloat : bytes

generateInstruction (IABoolean b) = do
    return $ Right [opcodeToByte OpPushBool, if b then 0x01 else 0x00]

generateInstruction (IAString s) = do
    idx <- addConstant (ConstString s)
    let idxBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
    return $ Right $ opcodeToByte OpPushConst : idxBytes

generateInstruction (IASymbol sym) = do
    idx <- addConstant (ConstSymbol sym)
    let idxBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
    return $ Right $ opcodeToByte OpLoad : idxBytes

generateInstruction (IAUnit) = do
    return $ Right [opcodeToByte OpPushNil]

generateInstruction (IAList exprs) = do
    results <- mapM generateInstruction exprs
    case sequence results of
        Left err -> return $ Left err
        Right codes -> do
            let listCode = opcodeToByte OpList : BL.unpack (runPut $ putWord8 (fromIntegral $ length exprs))
            return $ Right $ concat codes ++ listCode

generateInstruction (IAInfix left op right) = do
    leftResult <- generateInstruction left
    rightResult <- generateInstruction right
    case (leftResult, rightResult) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right leftCode, Right rightCode) -> do
            let opCode = case op of
                    "+" -> Right [opcodeToByte OpAdd]
                    "-" -> Right [opcodeToByte OpSub]
                    "*" -> Right [opcodeToByte OpMul]
                    "/" -> Right [opcodeToByte OpDiv]
                    "%" -> Right [opcodeToByte OpMod]
                    "==" -> Right [opcodeToByte OpEq]
                    "!=" -> Right [opcodeToByte OpNeq]
                    "<" -> Right [opcodeToByte OpLt]
                    ">" -> Right [opcodeToByte OpGt]
                    "<=" -> Right [opcodeToByte OpLte]
                    ">=" -> Right [opcodeToByte OpGte]
                    "et" -> Right [opcodeToByte OpAnd]
                    "ou" -> Right [opcodeToByte OpOr]
                    _ -> Left $ "Unknown operator: " ++ op
            case opCode of
                Left err -> return $ Left err
                Right code -> return $ Right $ leftCode ++ rightCode ++ code

generateInstruction (IADeclare name _ expr) = do
    exprResult <- generateInstruction expr
    case exprResult of
        Left err -> return $ Left err
        Right exprCode -> do
            idx <- addConstant (ConstSymbol name)
            let storeBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
            return $ Right $ exprCode ++ (opcodeToByte OpDefine : storeBytes)

generateInstruction (IAAssign name expr) = do
    exprResult <- generateInstruction expr
    case exprResult of
        Left err -> return $ Left err
        Right exprCode -> do
            idx <- addConstant (ConstSymbol name)
            let storeBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
            return $ Right $ exprCode ++ (opcodeToByte OpStore : storeBytes)

generateInstruction (IACall "afficher" args) = do
    results <- mapM generateInstruction args
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes ++ [opcodeToByte OpPrint]

generateInstruction (IACall fname args) = do
    results <- mapM generateInstruction args
    case sequence results of
        Left err -> return $ Left err
        Right codes -> do
            idx <- addConstant (ConstSymbol fname)
            let funcBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
            let argCountByte = fromIntegral $ length args
            return $ Right $ concat codes ++ (opcodeToByte OpCall : funcBytes ++ [argCountByte])

generateInstruction (IAReturn expr) = do
    exprResult <- generateInstruction expr
    case exprResult of
        Left err -> return $ Left err
        Right exprCode -> return $ Right $ exprCode ++ [opcodeToByte OpReturn]

generateInstruction (IAIf condExpr thenStmts maybeElseStmts) = do
    condResult <- generateInstruction condExpr
    thenResults <- mapM generateInstruction thenStmts

    case (condResult, sequence thenResults) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right condCode, Right thenCodes) -> do
            let thenBody = concat thenCodes

            case maybeElseStmts of
                Nothing -> do
                    let thenLength = length thenBody
                    let jmpOffset = fromIntegral thenLength
                    let jmpBytes = BL.unpack $ runPut $ putWord32be jmpOffset
                    return $ Right $ condCode ++ (opcodeToByte OpJmpIfFalse : jmpBytes) ++ thenBody

                Just elseStmts -> do
                    elseResults <- mapM generateInstruction elseStmts
                    case sequence elseResults of
                        Left err -> return $ Left err
                        Right elseCodes -> do
                            let elseBody = concat elseCodes
                            let thenLength = length thenBody + 5  -- +5 pour le Jmp à la fin du then
                            let elseLength = length elseBody
                            let jmpIfFalseOffset = fromIntegral thenLength
                            let jmpOffset = fromIntegral elseLength
                            let jmpIfFalseBytes = BL.unpack $ runPut $ putWord32be jmpIfFalseOffset
                            let jmpBytes = BL.unpack $ runPut $ putWord32be jmpOffset
                            return $ Right $ condCode
                                ++ (opcodeToByte OpJmpIfFalse : jmpIfFalseBytes)
                                ++ thenBody
                                ++ (opcodeToByte OpJmp : jmpBytes)
                                ++ elseBody

generateInstruction (IAWhile condExpr bodyStmts) = do
    condResult <- generateInstruction condExpr
    bodyResults <- mapM generateInstruction bodyStmts

    case (condResult, sequence bodyResults) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right condCode, Right bodyCodes) -> do
            let body = concat bodyCodes
            let condLength = length condCode
            let bodyLength = length body
            -- JmpIfFalse pour sortir de la boucle
            let jmpIfFalseOffset = fromIntegral (bodyLength + 5)  -- +5 pour le Jmp de retour
            -- Jmp pour retourner au début de la condition
            let jmpBackOffset = fromIntegral (condLength + bodyLength + 5)
            let jmpIfFalseBytes = BL.unpack $ runPut $ putWord32be jmpIfFalseOffset
            let jmpBackBytes = BL.unpack $ runPut $ putWord32be jmpBackOffset
            return $ Right $ condCode
                ++ (opcodeToByte OpJmpIfFalse : jmpIfFalseBytes)
                ++ body
                ++ (opcodeToByte OpJmp : jmpBackBytes)

generateInstruction (IAFor iterVar startExpr endExpr bodyStmts) = do
    initResult <- generateInstruction (IAAssign (getSymbolName iterVar) startExpr)

    endResult <- generateInstruction endExpr
    condResult <- generateInstruction (IAInfix iterVar "<=" endExpr)

    bodyResults <- mapM generateInstruction bodyStmts

    let incrExpr = IAAssign (getSymbolName iterVar)
                            (IAInfix iterVar "+" (IANumber 1))
    incrResult <- generateInstruction incrExpr

    case (initResult, endResult, condResult, sequence bodyResults, incrResult) of
        (Left err, _, _, _, _) -> return $ Left err
        (_, Left err, _, _, _) -> return $ Left err
        (_, _, Left err, _, _) -> return $ Left err
        (_, _, _, Left err, _) -> return $ Left err
        (_, _, _, _, Left err) -> return $ Left err
        (Right initCode, Right endCode, Right condCode, Right bodyCodes, Right incrCode) -> do
            let body = concat bodyCodes
            let condLength = length condCode
            let bodyLength = length body
            let incrLength = length incrCode
            -- JmpIfFalse pour sortir de la boucle
            let jmpIfFalseOffset = fromIntegral (bodyLength + incrLength + 5)
            -- Jmp pour retourner au début de la condition
            let jmpBackOffset = fromIntegral (condLength + bodyLength + incrLength + 5)
            let jmpIfFalseBytes = BL.unpack $ runPut $ putWord32be jmpIfFalseOffset
            let jmpBackBytes = BL.unpack $ runPut $ putWord32be jmpBackOffset
            return $ Right $ initCode
                ++ condCode
                ++ (opcodeToByte OpJmpIfFalse : jmpIfFalseBytes)
                ++ body
                ++ incrCode
                ++ (opcodeToByte OpJmp : jmpBackBytes)
  where
    getSymbolName (IASymbol name) = name
    getSymbolName _ = "i"  -- Valeur par défaut

generateInstruction (IABlock stmts) = do
    results <- mapM generateInstruction stmts
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes

generateInstruction (IAMain stmts) = do
    results <- mapM generateInstruction stmts
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes

generateInstruction (IAFunctionDef name args _retType body) = do
    results <- mapM generateInstruction body
    case sequence results of
        Left err -> return $ Left err
        Right bodyCodes -> do
            funcIdx <- addConstant (ConstSymbol name)

            let currentOffset = 0

            ctx <- get
            let funcEntry = FunctionEntry
                    { funcIndex = nextFuncIndex ctx
                    , funcAddress = currentOffset
                    , funcArgCount = length args
                    }
            put ctx { funcTable = funcTable ctx ++ [funcEntry]
                    , funcMap = Map.insert name (nextFuncIndex ctx) (funcMap ctx)
                    , nextFuncIndex = nextFuncIndex ctx + 1
                    }

            let funcIndexBytes = BL.unpack $ runPut $ putWord32be (fromIntegral funcIdx)
            let argCountByte = fromIntegral $ length args
            let bodyCode = concat bodyCodes

            let hasReturn = not (null bodyCode) && last bodyCode == opcodeToByte OpReturn
            let finalBody = if hasReturn then bodyCode else bodyCode ++ [opcodeToByte OpReturn]

            return $ Right $ opcodeToByte OpClosure : funcIndexBytes ++ [argCountByte] ++ finalBody

generateInstruction (IAProgram stmts) = do
    results <- mapM generateInstruction stmts
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes ++ [opcodeToByte OpHalt]

generateInstruction ast = return $ Left $ "Unsupported AST node: " ++ show ast

-- ==========================
-- Fonction principale
-- ==========================

parseBin :: AST -> String -> IO (Either String ())
parseBin ast outputName = do
    let (instructionsResult, finalCtx) = runState (generateInstruction ast) emptyContext

    case instructionsResult of
        Left err -> return $ Left $ "Bytecode generation error: " ++ err
        Right instructions -> do
            let bytecode = runPut $ do
                    generateHeader
                    generateConstantPool (constPool finalCtx)
                    generateFunctionTable (funcTable finalCtx)
                    putWord32be (fromIntegral $ length instructions)
                    mapM_ putWord8 instructions

            BS.writeFile outputName (BL.toStrict bytecode)
            return $ Right ()