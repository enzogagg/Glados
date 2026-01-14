{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-2-glados-1
-- File description:
-- Bytecode Instructions Generation
-}

module Bytecode.Instructions (
    generateInstruction,
    generateInstructionWithCount,
    countInstructions
) where

import Types
import Bytecode.Context
import Data.Word
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Control.Monad.State
import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import Data.Maybe(fromMaybe)

-- ==========================
-- Helper Functions
-- ==========================

-- Fonction helper pour générer des instructions et mettre à jour instructionCount
generateInstructionWithCount :: AST -> CodeGen
generateInstructionWithCount ast = do
    result <- generateInstruction ast
    case result of
        Left err -> return $ Left err
        Right code -> do
            modify $ \c -> c { instructionCount = instructionCount c + countInstructions code }
            return $ Right code

countInstructions :: [Word8] -> Int
countInstructions [] = 0
countInstructions (opcode:rest) = 1 + countInstructions (drop (getOpcodeSize opcode) rest)

inferType :: AST -> State BytecodeContext CladType
inferType (IASymbol s) = do
    ctx <- get
    case Map.lookup s (variableTypes ctx) of
        Just t -> return t
        Nothing -> return AnyT
inferType (IACall "vector" _) = return (ArrayT AnyT)
inferType (IACall "dico" _) = return (MapT AnyT AnyT)
inferType (IACall "struct" _) = return StructT
inferType (IATuple _) = return (TupleT [])
inferType _ = return AnyT

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
            let listCode = opcodeToByte OpList : BL.unpack (runPut $ putWord32be (fromIntegral $ length exprs))
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
                    "div" -> Right [opcodeToByte OpDiv]
                    "%" -> Right [opcodeToByte OpMod]
                    "mod" -> Right [opcodeToByte OpMod]
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
genDeclare (IADeclare name typeAnnot expr) = Just $ do
    exprResult <- generateInstruction expr
    case exprResult of
        Left err -> return $ Left err
        Right exprCode -> do
            ctx <- get
            let varType = fromMaybe AnyT typeAnnot
            put ctx { variableTypes = Map.insert name varType (variableTypes ctx) }

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

genCallEcouter :: AST -> Maybe CodeGen
genCallEcouter (IACall "ecouter" []) = Just $ return $ Right [opcodeToByte OpInput]
genCallEcouter _ = Nothing

genCallNot :: AST -> Maybe CodeGen
genCallNot (IACall "!" [arg]) = Just $ do
    argResult <- generateInstruction arg
    case argResult of
        Left err -> return $ Left err
        Right argCode -> return $ Right $ argCode ++ [opcodeToByte OpNot]
genCallNot _ = Nothing


-- ==========================
-- Operations Unifiées (Get/Set pour tout)
-- ==========================

genCallCommon :: AST -> Maybe CodeGen
genCallCommon (IACall name args) = case name of
    "vector" -> Just $ do
            results <- mapM generateInstruction args
            case sequence results of
                Left err -> return $ Left err
                Right codes -> do
                    let vectorCode = opcodeToByte OpMakeArray : BL.unpack (runPut $ putWord32be (fromIntegral $ length args))
                    return $ Right $ concat codes ++ vectorCode

    "dico" -> Just $ do
            if odd (length args)
                then return $ Left "Function 'dico' expects an even number of arguments (pairs key-value)"
                else do
                    results <- mapM generateInstruction (reverse args)
                    case sequence results of
                        Left err -> return $ Left err
                        Right codes -> do
                            let numPairs = length args `div` 2
                            let mapCode = opcodeToByte OpMakeMap : BL.unpack (runPut $ putWord32be (fromIntegral numPairs))
                            return $ Right $ concat codes ++ mapCode

    "struct" -> Just $ do
             if odd (length args)
                then return $ Left "Function 'struct' expects an even number of arguments (pairs key-value)"
                else do
                    results <- mapM generateInstruction (reverse args)
                    case sequence results of
                        Left err -> return $ Left err
                        Right codes -> do
                            let count = length args `div` 2
                            let structCode = opcodeToByte OpMakeStruct : BL.unpack (runPut $ putWord32be (fromIntegral count))
                            return $ Right $ concat codes ++ structCode

    "get" -> Just $ case args of
        [obj, idx] -> do
            objType <- inferType obj
            case objType of
                ArrayT _ -> genStackGet OpArrayGet obj idx
                ListT _ -> genStackGet OpArrayGet obj idx
                MapT _ _ -> genStackGet OpMapGet obj idx

                TupleT _ -> case idx of
                    IANumber n -> genImmediateGet OpTupleGet n obj
                    _ -> return $ Left "Tuple get requires integer constant index"

                StructT -> case idx of
                    IAString s -> genSymbolGet OpStructGet s obj
                    _ -> return $ Left "Struct get requires string constant key"

                AnyT -> case idx of
                    IANumber _ -> genStackGet OpArrayGet obj idx
                    IAString _ -> genStackGet OpMapGet obj idx
                    _ -> genStackGet OpArrayGet obj idx

                _ -> return $ Left $ "Type " ++ show objType ++ " does not support 'get'"
        _ -> return $ Left "Function 'get' expects 2 arguments (object, index/key)"

    "set" -> Just $ case args of
        [obj, idx, val] -> do
            objType <- inferType obj
            case objType of
                ArrayT _ -> genStackSet OpArraySet obj idx val
                MapT _ _ -> genStackSet OpMapSet obj idx val

                StructT -> case idx of
                    IAString s -> genSymbolSet OpStructSet s obj val
                    _ -> return $ Left "Struct set requires string constant key"

                AnyT -> case idx of
                    IANumber _ -> genStackSet OpArraySet obj idx val
                    IAString _ -> genStackSet OpMapSet obj idx val
                    _ -> genStackSet OpArraySet obj idx val

                _ -> return $ Left $ "Type " ++ show objType ++ " does not support 'set'"
        _ -> return $ Left "Function 'set' expects 3 arguments (object, index/key, value)"

    "open" -> Just $ do
        if length args /= 2
            then return $ Left "Function 'open' expects 2 arguments (path, mode)"
            else do
                results <- mapM generateInstruction args
                case sequence results of
                    Left err -> return $ Left err
                    Right codes -> return $ Right $ concat codes ++ [opcodeToByte OpOpenFile]

    "read" -> Just $ do
        if length args /= 1
            then return $ Left "Function 'read' expects 1 argument (file)"
            else do
                results <- mapM generateInstruction args
                case sequence results of
                    Left err -> return $ Left err
                    Right codes -> return $ Right $ concat codes ++ [opcodeToByte OpReadFile]

    "write" -> Just $ do
        if length args /= 2
            then return $ Left "Function 'write' expects 2 arguments (file, content)"
            else do
                 results <- mapM generateInstruction args
                 case sequence results of
                    Left err -> return $ Left err
                    Right codes -> return $ Right $ concat codes ++ [opcodeToByte OpWriteFile]

    "close" -> Just $ do
        if length args /= 1
            then return $ Left "Function 'close' expects 1 argument (file)"
            else do
                results <- mapM generateInstruction args
                case sequence results of
                    Left err -> return $ Left err
                    Right codes -> return $ Right $ concat codes ++ [opcodeToByte OpCloseFile]

    _ -> Nothing

    where
        genStackGet op obj idx = do
            r1 <- generateInstruction obj
            r2 <- generateInstruction idx
            case (r1, r2) of
                (Right c1, Right c2) -> return $ Right $ c1 ++ c2 ++ [opcodeToByte op]
                _ -> return $ Left "Error generating get arguments"

        genImmediateGet op idx obj = do
            r1 <- generateInstruction obj
            case r1 of
                Right c1 -> do
                    let bytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
                    return $ Right $ c1 ++ (opcodeToByte op : bytes)
                Left e -> return $ Left e

        genSymbolGet op sym obj = do
            r1 <- generateInstruction obj
            case r1 of
                Right c1 -> do
                    idx <- addConstant (ConstString sym)
                    let bytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
                    return $ Right $ c1 ++ (opcodeToByte op : bytes)
                Left e -> return $ Left e

        genStackSet op obj idx val = do
            r1 <- generateInstruction obj
            r2 <- generateInstruction idx
            r3 <- generateInstruction val
            case (r1, r2, r3) of
                (Right c1, Right c2, Right c3) -> return $ Right $ c1 ++ c2 ++ c3 ++ [opcodeToByte op]
                _ -> return $ Left "Error generating set arguments"

        genSymbolSet op sym obj val = do
            r1 <- generateInstruction obj
            r3 <- generateInstruction val
            case (r1, r3) of
                 (Right c1, Right c3) -> do
                    idx <- addConstant (ConstString sym)
                    let bytes = BL.unpack $ runPut $ putWord32be (fromIntegral idx)
                    return $ Right $ c1 ++ c3 ++ (opcodeToByte op : bytes)
                 _ -> return $ Left "Error generating set"

genCallCommon _ = Nothing


genCallListOps :: AST -> Maybe CodeGen
genCallListOps (IACall name args) =
    case name of
        "cons" -> checkArgs 2 OpCons
        "head" -> checkArgs 1 OpHead
        "tail" -> checkArgs 1 OpTail
        "len" -> checkArgs 1 OpLen
        "is_empty" -> checkArgs 1 OpIsEmpty
        "nth" -> checkArgs 2 OpNth
        "append" -> checkArgs 2 OpAppend
        "insert" -> checkArgs 3 OpInsert
        "remove" -> checkArgs 2 OpRemove
        "contains" -> checkArgs 2 OpContains
        "reverse" -> checkArgs 1 OpReverse
        _ -> Nothing
    where
        checkArgs count op =
            if length args == count
            then Just $ do
                let argsToGen = case name of
                        "contains" -> reverse args
                        "insert" -> reverse args
                        "nth" -> reverse args
                        "remove" -> reverse args
                        _ -> args

                results <- mapM generateInstruction argsToGen
                case sequence results of
                    Left err -> return $ Left err
                    Right codes -> return $ Right $ concat codes ++ [opcodeToByte op]
            else Just $ return $ Left $ "Function '" ++ name ++ "' expects " ++ show count ++ " arguments"
genCallListOps _ = Nothing

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
    startInstr <- gets instructionCount

    code <- do
        condResult <- generateInstructionWithCount condExpr
        modify $ \c -> c { instructionCount = instructionCount c + 1 }

        thenResults <- mapM generateInstructionWithCount thenStmts

        case maybeElseStmts of
            Nothing -> do
                endInstr <- gets instructionCount

                case (condResult, sequence thenResults) of
                    (Right condCode, Right thenCodes) -> do
                        let jmpBytes = BL.unpack $ runPut $ putWord32be (fromIntegral endInstr)
                        return $ Right $ condCode ++ (opcodeToByte OpJmpIfFalse : jmpBytes) ++ concat thenCodes
                    (Left err, _) -> return $ Left err
                    (_, Left err) -> return $ Left err

            Just elseStmts -> do
                modify $ \c -> c { instructionCount = instructionCount c + 1 }
                elseResults <- mapM generateInstructionWithCount elseStmts
                endInstr <- gets instructionCount

                case (condResult, sequence thenResults, sequence elseResults) of
                    (Right condCode, Right thenCodes, Right elseCodes) -> do
                        let condLen = countInstructions condCode
                        let thenLen = countInstructions (concat thenCodes)

                        let elseStart = startInstr + condLen + 1 + thenLen + 1
                        let jmpIfFalseBytes = BL.unpack $ runPut $ putWord32be (fromIntegral elseStart)

                        let jmpBytes = BL.unpack $ runPut $ putWord32be (fromIntegral endInstr)

                        return $ Right $ condCode
                            ++ (opcodeToByte OpJmpIfFalse : jmpIfFalseBytes)
                            ++ concat thenCodes
                            ++ (opcodeToByte OpJmp : jmpBytes)
                            ++ concat elseCodes
                    (Left err, _, _) -> return $ Left err
                    (_, Left err, _) -> return $ Left err
                    (_, _, Left err) -> return $ Left err

    modify $ \c -> c { instructionCount = startInstr }
    return code
genIf _ = Nothing

genWhile :: AST -> Maybe CodeGen
genWhile (IAWhile condExpr bodyStmts) = Just $ do
    startInstr <- gets instructionCount

    code <- do
        condResult <- generateInstructionWithCount condExpr
        modify $ \c -> c { instructionCount = instructionCount c + 1 }

        bodyResults <- mapM generateInstructionWithCount bodyStmts
        modify $ \c -> c { instructionCount = instructionCount c + 1 }

        case (condResult, sequence bodyResults) of
            (Left err, _) -> return $ Left err
            (_, Left err) -> return $ Left err
            (Right condCode, Right bodyCodes) -> do
                let body = concat bodyCodes
                let condInstrCount = countInstructions condCode
                let bodyInstrCount = countInstructions body

                let jmpIfFalseTarget = fromIntegral (startInstr + condInstrCount + 1 + bodyInstrCount + 1)
                let jmpBackTarget = fromIntegral startInstr

                let jmpIfFalseBytes = BL.unpack $ runPut $ putWord32be jmpIfFalseTarget
                let jmpBackBytes = BL.unpack $ runPut $ putWord32be jmpBackTarget

                return $ Right $ condCode
                    ++ (opcodeToByte OpJmpIfFalse : jmpIfFalseBytes)
                    ++ body
                    ++ (opcodeToByte OpJmp : jmpBackBytes)

    modify $ \c -> c { instructionCount = startInstr }
    return code
genWhile _ = Nothing

genFor :: AST -> Maybe CodeGen
genFor (IAFor initExpr condExpr incrExpr bodyStmts) = Just $ do
    startInstr <- gets instructionCount

    code <- do
        initResult <- generateInstructionWithCount initExpr

        loopStartInstr <- gets instructionCount

        condResult <- generateInstructionWithCount condExpr
        modify $ \c -> c { instructionCount = instructionCount c + 1 }

        bodyResults <- mapM generateInstructionWithCount bodyStmts

        incrResult <- generateInstructionWithCount incrExpr

        modify $ \c -> c { instructionCount = instructionCount c + 1 }

        case (initResult, condResult, sequence bodyResults, incrResult) of
            (Right initCode, Right condCode, Right bodyCodes, Right incrCode) -> do
                let body = concat bodyCodes

                endInstr <- gets instructionCount

                let jmpIfFalseBytes = BL.unpack $ runPut $ putWord32be (fromIntegral endInstr)
                let jmpBackBytes = BL.unpack $ runPut $ putWord32be (fromIntegral loopStartInstr)

                return $ Right $ initCode
                    ++ condCode
                    ++ (opcodeToByte OpJmpIfFalse : jmpIfFalseBytes)
                    ++ body
                    ++ incrCode
                    ++ (opcodeToByte OpJmp : jmpBackBytes)

            (Left err, _, _, _) -> return $ Left err
            (_, Left err, _, _) -> return $ Left err
            (_, _, Left err, _) -> return $ Left err
            (_, _, _, Left err) -> return $ Left err

    modify $ \c -> c { instructionCount = startInstr }
    return code
genFor _ = Nothing

genBlock :: AST -> Maybe CodeGen
genBlock (IABlock stmts) = Just $ do
    startInstr <- gets instructionCount
    results <- mapM generateInstructionWithCount stmts
    modify $ \c -> c { instructionCount = startInstr }
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes
genBlock _ = Nothing

genMain :: AST -> Maybe CodeGen
genMain (IAMain _ stmts) = Just $ do
    startInstr <- gets instructionCount
    results <- mapM generateInstructionWithCount stmts
    modify $ \c -> c { instructionCount = startInstr }
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
    let oldVarTypes = variableTypes ctx

    let paramNames = map fst args
    let paramsList = zip paramNames [0..]

    let paramTypes = map (\(n, t) -> (n, fromMaybe AnyT t)) args
    let newVarTypes = foldl (\m (n, t) -> Map.insert n t m) oldVarTypes paramTypes

    ctx' <- get
    put ctx' { currentFuncParams = paramsList, variableTypes = newVarTypes }

    results <- mapM generateInstructionWithCount body

    case sequence results of
        Left err -> do
            ctx'' <- get
            put ctx'' { currentFuncParams = oldParams, variableTypes = oldVarTypes }
            return $ Left err
        Right bodyCodes -> do
            ctx'' <- get
            put ctx'' { currentFuncParams = oldParams, variableTypes = oldVarTypes }

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

genProgram :: AST -> Maybe CodeGen
genProgram (IAProgram stmts) = Just $ do
    results <- mapM generateInstructionWithCount stmts
    case sequence results of
        Left err -> return $ Left err
        Right codes -> return $ Right $ concat codes ++ [opcodeToByte OpHalt]
genProgram _ = Nothing

-- ==========================
-- Dispatcher principal
-- ==========================

generateInstruction :: AST -> CodeGen
generateInstruction ast =
    fromMaybe unsupported $
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
        <|> genCallEcouter ast
        <|> genCallNot ast
        <|> genCallCommon ast
        <|> genCallListOps ast
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
