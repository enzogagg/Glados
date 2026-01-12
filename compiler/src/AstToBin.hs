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
import Control.Applicative ((<|>))
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
    , currentFuncParams :: [(String, Int)]
    , instructionCount :: Int
    } deriving (Show)

emptyContext :: BytecodeContext
emptyContext = BytecodeContext [] Map.empty [] Map.empty 0 0 [] 0

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
    putWord32be 4  -- Float32 = 4 bytes (pas 8)
    putFloatbe (realToFrac f)

putConstantEntry (ConstBool b) = do
    putWord8 (typeTagToByte TagBool)
    putWord32be 1
    putWord8 (if b then 0x01 else 0x00)

putConstantEntry (ConstChar c) = do
    putWord8 (typeTagToByte TagChar)
    putWord32be 1
    putWord8 (fromIntegral $ fromEnum c)

putConstantEntry (ConstString s) = do
    putWord8 (typeTagToByte TagString)
    let bytes = map (fromIntegral . fromEnum) s
    putWord32be (fromIntegral $ length bytes)
    mapM_ putWord8 bytes

putConstantEntry (ConstList entries) = do
    putWord8 (typeTagToByte TagList)
    let entriesBytes = BL.toStrict $ runPut $ mapM_ putConstantEntry entries
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes

putConstantEntry (ConstSymbol s) = do
    putWord8 (typeTagToByte TagSymbol)
    let bytes = map (fromIntegral . fromEnum) s
    putWord32be (fromIntegral $ length bytes)
    mapM_ putWord8 bytes

putConstantEntry ConstNil = do
    putWord8 (typeTagToByte TagNil)
    putWord32be 0

putConstantEntry (ConstTuple entries) = do
    putWord8 (typeTagToByte TagTuple)
    let entriesBytes = BL.toStrict $ runPut $ do
            putWord32be (fromIntegral $ length entries)
            mapM_ putConstantEntry entries
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes

putConstantEntry (ConstArray entries) = do
    putWord8 (typeTagToByte TagArray)
    let entriesBytes = BL.toStrict $ runPut $ do
            putWord32be (fromIntegral $ length entries)
            mapM_ putConstantEntry entries
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes

putConstantEntry (ConstStruct fields) = do
    putWord8 (typeTagToByte TagStruct)
    let entriesBytes = BL.toStrict $ runPut $ do
            putWord32be (fromIntegral $ length fields)
            mapM_ (\(name, value) -> do
                let nameBytes = map (fromIntegral . fromEnum) name
                putWord32be (fromIntegral $ length nameBytes)
                mapM_ putWord8 nameBytes
                putConstantEntry value) fields
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes

putConstantEntry (ConstMap pairs) = do
    putWord8 (typeTagToByte TagMap)
    let entriesBytes = BL.toStrict $ runPut $ do
            putWord32be (fromIntegral $ length pairs)
            mapM_ (\(key, value) -> do
                putConstantEntry key
                putConstantEntry value) pairs
    putWord32be (fromIntegral $ BS.length entriesBytes)
    putByteString entriesBytes

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

-- Fonction helper pour générer des instructions et mettre à jour instructionCount
generateInstructionWithCount :: AST -> CodeGen
generateInstructionWithCount ast = do
    result <- generateInstruction ast
    case result of
        Left err -> return $ Left err
        Right code -> do
            modify $ \c -> c { instructionCount = instructionCount c + countInstructions code }
            return $ Right code

-- ==========================
-- Générateurs par type d'AST
-- ==========================

genNumber :: AST -> Maybe CodeGen
genNumber (IANumber n) = Just $ do
    if n >= -2147483648 && n <= 2147483647
        then do
            let bytes = BL.unpack $ runPut $ putWord32be (fromIntegral n)
            return $ Right $ opcodeToByte OpPushInt : bytes
        else return $ Left $ "Integer value out of range (must fit in 4 bytes): " ++ show n
genNumber _ = Nothing

genFloat :: AST -> Maybe CodeGen
genFloat (IAFloatLiteral f) = Just $ do
    let bytes = BL.unpack $ runPut $ putFloatbe (realToFrac f)
    return $ Right $ opcodeToByte OpPushFloat : bytes
genFloat _ = Nothing

genBoolean :: AST -> Maybe CodeGen
genBoolean (IABoolean b) = Just $ return $ Right [opcodeToByte OpPushBool, if b then 0x01 else 0x00]
genBoolean _ = Nothing

genString :: AST -> Maybe CodeGen
genString (IAString s) = Just $ do
    idx <- addConstant (ConstString s)
    let idxBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
    return $ Right $ opcodeToByte OpPushConst : idxBytes
genString _ = Nothing

genSymbol :: AST -> Maybe CodeGen
genSymbol (IASymbol sym) = Just $ do
    ctx <- get
    case lookup sym (currentFuncParams ctx) of
        Just argIndex -> do
            let idxBytes = BL.unpack $ runPut $ putWord32be (fromIntegral argIndex)
            return $ Right $ opcodeToByte OpLoadArg : idxBytes
        Nothing -> do
            idx <- addConstant (ConstSymbol sym)
            let idxBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
            return $ Right $ opcodeToByte OpLoad : idxBytes
genSymbol _ = Nothing

genUnit :: AST -> Maybe CodeGen
genUnit IAUnit = Just $ return $ Right [opcodeToByte OpPushNil]
genUnit _ = Nothing

genList :: AST -> Maybe CodeGen
genList (IAList exprs) = Just $ do
    results <- mapM generateInstruction exprs
    case sequence results of
        Left err -> return $ Left err
        Right codes -> do
            let listCode = opcodeToByte OpList : BL.unpack (runPut $ putWord8 (fromIntegral $ length exprs))
            return $ Right $ concat codes ++ listCode
genList _ = Nothing

genTuple :: AST -> Maybe CodeGen
genTuple (IATuple exprs) = Just $ do
    results <- mapM generateInstruction exprs
    case sequence results of
        Left err -> return $ Left err
        Right codes -> do
            let tupleCode = opcodeToByte OpMakeTuple : BL.unpack (runPut $ putWord32be (fromIntegral $ length exprs))
            return $ Right $ concat codes ++ tupleCode
genTuple _ = Nothing

genInfix :: AST -> Maybe CodeGen
genInfix (IAInfix left op right) = Just $ do
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
genInfix _ = Nothing

genDeclare :: AST -> Maybe CodeGen
genDeclare (IADeclare name _ expr) = Just $ do
    exprResult <- generateInstruction expr
    case exprResult of
        Left err -> return $ Left err
        Right exprCode -> do
            idx <- addConstant (ConstSymbol name)
            let storeBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
            return $ Right $ exprCode ++ (opcodeToByte OpDefine : storeBytes)
genDeclare _ = Nothing

genAssign :: AST -> Maybe CodeGen
genAssign (IAAssign name expr) = Just $ do
    exprResult <- generateInstruction expr
    case exprResult of
        Left err -> return $ Left err
        Right exprCode -> do
            idx <- addConstant (ConstSymbol name)
            let storeBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
            return $ Right $ exprCode ++ (opcodeToByte OpStore : storeBytes)
genAssign _ = Nothing

genCallAfficher :: AST -> Maybe CodeGen
genCallAfficher (IACall "afficher" args) = Just $ do
    results <- mapM generateInstruction args
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes ++ [opcodeToByte OpPrint]
genCallAfficher _ = Nothing

genCallNot :: AST -> Maybe CodeGen
genCallNot (IACall "!" [arg]) = Just $ do
    argResult <- generateInstruction arg
    case argResult of
        Left err -> return $ Left err
        Right argCode -> return $ Right $ argCode ++ [opcodeToByte OpNot]
genCallNot _ = Nothing

genCall :: AST -> Maybe CodeGen
genCall (IACall fname args) = Just $ do
    results <- mapM generateInstruction args
    case sequence results of
        Left err -> return $ Left err
        Right codes -> do
            ctx <- get
            case Map.lookup fname (funcMap ctx) of
                Just funcTableIdx -> do
                    let funcBytes = BL.unpack $ runPut $ putWord32be (fromIntegral funcTableIdx)
                    let argCountByte = fromIntegral $ length args
                    return $ Right $ concat codes ++ (opcodeToByte OpCall : funcBytes ++ [argCountByte])
                Nothing -> do
                    idx <- addConstant (ConstSymbol fname)
                    let funcBytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
                    let argCountByte = fromIntegral $ length args
                    return $ Right $ concat codes ++ (opcodeToByte OpCall : funcBytes ++ [argCountByte])
genCall _ = Nothing

genReturn :: AST -> Maybe CodeGen
genReturn (IAReturn expr) = Just $ do
    exprResult <- generateInstruction expr
    case exprResult of
        Left err -> return $ Left err
        Right exprCode -> return $ Right $ exprCode ++ [opcodeToByte OpReturn]
genReturn _ = Nothing

genIf :: AST -> Maybe CodeGen
genIf (IAIf condExpr thenStmts maybeElseStmts) = Just $ do
    ctx <- get
    let startInstr = instructionCount ctx
    
    condResult <- generateInstruction condExpr
    thenResults <- mapM generateInstruction thenStmts

    case (condResult, sequence thenResults) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right condCode, Right thenCodes) -> do
            let thenBody = concat thenCodes
            let condInstrCount = countInstructions condCode
            let thenInstrCount = countInstructions thenBody

            case maybeElseStmts of
                Nothing -> do
                    let jmpTarget = fromIntegral (startInstr + condInstrCount + 1 + thenInstrCount)
                    let jmpBytes = BL.unpack $ runPut $ putWord32be jmpTarget
                    modify $ \c -> c { instructionCount = startInstr + condInstrCount + 1 + thenInstrCount }
                    return $ Right $ condCode ++ (opcodeToByte OpJmpIfFalse : jmpBytes) ++ thenBody

                Just elseStmts -> do
                    elseResults <- mapM generateInstruction elseStmts
                    case sequence elseResults of
                        Left err -> return $ Left err
                        Right elseCodes -> do
                            let elseBody = concat elseCodes
                            let elseInstrCount = countInstructions elseBody
                            let jmpIfFalseTarget = fromIntegral (startInstr + condInstrCount + 1 + thenInstrCount + 1)
                            let jmpTarget = fromIntegral (startInstr + condInstrCount + 1 + thenInstrCount + 1 + elseInstrCount)
                            let jmpIfFalseBytes = BL.unpack $ runPut $ putWord32be jmpIfFalseTarget
                            let jmpBytes = BL.unpack $ runPut $ putWord32be jmpTarget
                            modify $ \c -> c { instructionCount = startInstr + condInstrCount + 1 + thenInstrCount + 1 + elseInstrCount }
                            return $ Right $ condCode
                                ++ (opcodeToByte OpJmpIfFalse : jmpIfFalseBytes)
                                ++ thenBody
                                ++ (opcodeToByte OpJmp : jmpBytes)
                                ++ elseBody
genIf _ = Nothing

genWhile :: AST -> Maybe CodeGen
genWhile (IAWhile condExpr bodyStmts) = Just $ do
    condResult <- generateInstruction condExpr
    bodyResults <- mapM generateInstruction bodyStmts

    case (condResult, sequence bodyResults) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right condCode, Right bodyCodes) -> do
            let body = concat bodyCodes
            let condLength = length condCode
            let bodyLength = length body
            let jmpIfFalseOffset = fromIntegral (bodyLength + 5)
            let jmpBackOffset = fromIntegral (condLength + bodyLength + 5)
            let jmpIfFalseBytes = BL.unpack $ runPut $ putWord32be jmpIfFalseOffset
            let jmpBackBytes = BL.unpack $ runPut $ putWord32be jmpBackOffset
            return $ Right $ condCode
                ++ (opcodeToByte OpJmpIfFalse : jmpIfFalseBytes)
                ++ body
                ++ (opcodeToByte OpJmp : jmpBackBytes)
genWhile _ = Nothing

genFor :: AST -> Maybe CodeGen
genFor (IAFor iterVar startExpr endExpr bodyStmts) = Just $ do
    let symbolName = getSymbolName iterVar
    initResult <- generateInstruction (IAAssign symbolName startExpr)
    _ <- generateInstruction endExpr
    condResult <- generateInstruction (IAInfix iterVar "<=" endExpr)
    bodyResults <- mapM generateInstruction bodyStmts
    let incrExpr = IAAssign symbolName (IAInfix iterVar "+" (IANumber 1))
    incrResult <- generateInstruction incrExpr

    case (initResult, condResult, sequence bodyResults, incrResult) of
        (Left err, _, _, _) -> return $ Left err
        (_, Left err, _, _) -> return $ Left err
        (_, _, Left err, _) -> return $ Left err
        (_, _, _, Left err) -> return $ Left err
        (Right initCode, Right condCode, Right bodyCodes, Right incrCode) -> do
            let body = concat bodyCodes
            let condLength = length condCode
            let bodyLength = length body
            let incrLength = length incrCode
            let jmpIfFalseOffset = fromIntegral (bodyLength + incrLength + 5)
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
        getSymbolName _ = "i"
genFor _ = Nothing

genBlock :: AST -> Maybe CodeGen
genBlock (IABlock stmts) = Just $ do
    results <- mapM generateInstruction stmts
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes
genBlock _ = Nothing

genMain :: AST -> Maybe CodeGen
genMain (IAMain args stmts) = Just $ do
    results <- mapM generateInstruction stmts
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes
genMain _ = Nothing

genFunctionDef :: AST -> Maybe CodeGen
genFunctionDef (IAFunctionDef name args _retType body) = Just $ do
    ctx <- get
    
    let funcTableIdx = nextFuncIndex ctx
    
    put ctx { funcMap = Map.insert name funcTableIdx (funcMap ctx)
            , nextFuncIndex = nextFuncIndex ctx + 1
            , instructionCount = 3
            }
    
    let oldParams = currentFuncParams ctx
    let paramNames = map fst args
    let paramsList = zip paramNames [0..]
    
    ctx' <- get
    put ctx' { currentFuncParams = paramsList }
    
    results <- mapM generateInstructionWithCount body
    
    case sequence results of
        Left err -> do
            ctx'' <- get
            put ctx'' { currentFuncParams = oldParams }
            return $ Left err
        Right bodyCodes -> do
            ctx'' <- get
            put ctx'' { currentFuncParams = oldParams }
            
            let bodyCode = concat bodyCodes
            let hasReturn = not (null bodyCode) && last bodyCode == opcodeToByte OpReturn
            let finalBody = if hasReturn then bodyCode else bodyCode ++ [opcodeToByte OpReturn]
            
            let currentOffset = 3
            
            let funcEntry = FunctionEntry
                    { funcIndex = funcTableIdx
                    , funcAddress = currentOffset
                    , funcArgCount = length args
                    }
            
            nameIdx <- addConstant (ConstSymbol name)
            let funcIndexBytes = BL.unpack $ runPut $ putWord32be (fromIntegral funcTableIdx)
            let defineBytes = BL.unpack $ runPut $ putWord32be (fromIntegral nameIdx)
            
            let bodyInstrCount = countInstructions finalBody
            let jmpTarget = fromIntegral (3 + bodyInstrCount) :: Word32
            let jmpBytes = BL.unpack $ runPut $ putWord32be jmpTarget
            
            ctx''' <- get
            put ctx''' { funcTable = funcTable ctx''' ++ [funcEntry] }

            return $ Right $ opcodeToByte OpClosure : funcIndexBytes 
                          ++ (opcodeToByte OpDefine : defineBytes)
                          ++ (opcodeToByte OpJmp : jmpBytes) 
                          ++ finalBody
genFunctionDef _ = Nothing

countInstructions :: [Word8] -> Int
countInstructions [] = 0
countInstructions (opcode:rest) = 1 + countInstructions (drop (getOpcodeSize opcode) rest)

adjustJumpAddresses :: Int -> [Word8] -> [Word8]
adjustJumpAddresses baseOffset bytes = adjustAtOffset 0 bytes
  where
    adjustAtOffset _ [] = []
    adjustAtOffset currentInstr (opcode:rest)
        | opcode == 0x60 || opcode == 0x61 || opcode == 0x62 =  -- JMP, JMP_IF_TRUE, JMP_IF_FALSE
            if length rest >= 4
            then let (addrBytes, remaining) = splitAt 4 rest
                     oldAddr = fromIntegral (foldl (\acc b -> acc * 256 + fromIntegral b) 0 addrBytes :: Int)
                     -- L'adresse est relative au début du IF, on ajoute baseOffset + position du IF
                     newAddr = oldAddr + baseOffset + currentInstr
                     newAddrBytes = BL.unpack $ runPut $ putWord32be (fromIntegral newAddr :: Word32)
                 in opcode : newAddrBytes ++ adjustAtOffset (currentInstr + 1) remaining
            else opcode : rest
        | otherwise =
            let size = getOpcodeSize opcode
                (operands, remaining) = splitAt size rest
            in opcode : operands ++ adjustAtOffset (currentInstr + 1) remaining

genProgram :: AST -> Maybe CodeGen
genProgram (IAProgram stmts) = Just $ do
    results <- mapM generateInstruction stmts
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes ++ [opcodeToByte OpHalt]
genProgram _ = Nothing

-- ==========================
-- Dispatcher principal
-- ==========================

generateInstruction :: AST -> CodeGen
generateInstruction ast =
    maybe unsupported id $
            genNumber ast
        <|> genFloat ast
        <|> genBoolean ast
        <|> genString ast
        <|> genSymbol ast
        <|> genUnit ast
        <|> genList ast
        <|> genTuple ast
        <|> genInfix ast
        <|> genDeclare ast
        <|> genAssign ast
        <|> genCallAfficher ast
        <|> genCallNot ast
        <|> genCall ast
        <|> genReturn ast
        <|> genIf ast
        <|> genWhile ast
        <|> genFor ast
        <|> genBlock ast
        <|> genMain ast
        <|> genFunctionDef ast
        <|> genProgram ast
  where
    unsupported = return $ Left $ "Unsupported AST node: " ++ show ast

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