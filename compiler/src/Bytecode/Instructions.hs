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

genCallListOps :: AST -> Maybe CodeGen
genCallListOps (IACall name args) =
    case name of
        "cons" -> checkArgs 2 OpCons
        "head" -> checkArgs 1 OpHead
        "tail" -> checkArgs 1 OpTail
        "len" -> checkArgs 1 OpLen
        _ -> Nothing
    where
        checkArgs count op =
            if length args == count
            then Just $ do
                results <- mapM generateInstruction args
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
genMain (IAMain _ stmts) = Just $ do
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
    Data.Maybe.fromMaybe unsupported $
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
