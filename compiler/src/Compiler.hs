module Compiler (compileProgram) where

import Bytecode
import Types (IAST(..))
import Control.Monad.State
import Data.List (elemIndex)
import qualified Data.Map as Map

-- Intermediate Instruction with Labels
data InterInstruction
    = RealInstr Instruction
    | Label String
    | JumpLabel String
    | JumpFalseLabel String
    | JumpTrueLabel String
    deriving (Show, Eq)

data CompileState = CompileState {
    interInstructions :: [InterInstruction],
    constantsPool :: [BCValue],
    functionsTable :: [FunctionMeta],
    funcMap :: Map.Map String Int, -- Name -> Index
    labelCounter :: Int,
    scopeStack :: [[String]] -- Stack of local parameter names
}

initialState :: CompileState
initialState = CompileState [] [] [] Map.empty 0 []

type Compiler = State CompileState

compileProgram :: IAST -> BytecodeFile
compileProgram (IAProgram iasts) =
    let
        -- Pass 1: Scan functions
        s1 = execState (scanFunctions iasts) initialState
        -- Pass 2: Compile logic
        
        mainBody = filter isMain iasts
        funcDefs = filter isFunction iasts
        otherStmts = filter (not . (\x -> isMain x || isFunction x)) iasts
        
        s2 = execState (do
            lMain <- freshLabel
            emit $ JumpLabel lMain
            mapM_ compile funcDefs
            emit $ Label lMain
            mapM_ compile otherStmts
            mapM_ compile mainBody
            emit $ RealInstr Halt
            ) s1
            
        (finalInstrs, labelMap) = resolveLabels (interInstructions s2)
        
        -- Resolve Function Addresses
        -- Invert funcMap: Name -> Id to Id -> Name
        idToName = Map.fromList [(i, n) | (n, i) <- Map.toList (funcMap s2)]
        
        updatedFuncs = map (\m -> 
            case Map.lookup (funcId m) idToName of
                Just name -> 
                    let label = "FUNC_" ++ name
                    in case Map.lookup label labelMap of
                        Just addr -> m { funcAddress = addr }
                        Nothing -> error $ "Function label not found: " ++ label
                Nothing -> error $ "Function Name lookup failed for ID: " ++ show (funcId m)
            ) (functionsTable s2)
            
    in BytecodeFile 0x43424300 1 0 (constantsPool s2) updatedFuncs finalInstrs
compileProgram _ = error "Expected IAProgram"

resolveLabels :: [InterInstruction] -> ([Instruction], Map.Map String Int)
resolveLabels instrs =
    let
        (labelMap, _, _) = foldl buildMap (Map.empty, 0, []) instrs

        buildMap (m, addr, acc) (Label s) = (Map.insert s addr m, addr, acc)
        buildMap (m, addr, acc) _ = (m, addr + 1, acc)

        resolve (RealInstr i) = i
        resolve (Label _) = error "Should be removed"
        resolve (JumpLabel s) = Jump (lookupLabel s labelMap)
        resolve (JumpFalseLabel s) = JumpIfFalse (lookupLabel s labelMap)
        resolve (JumpTrueLabel s) = JumpIfTrue (lookupLabel s labelMap)

        lookupLabel s m = case Map.lookup s m of
            Just a -> a
            Nothing -> error $ "Label not found: " ++ s
            
    in (map resolve $ filter (not . isLabel) instrs, labelMap)

isLabel :: InterInstruction -> Bool
isLabel (Label _) = True
isLabel _ = False

isMain :: IAST -> Bool
isMain (IAMain _) = True
isMain _ = False

isFunction :: IAST -> Bool
isFunction (IAFunctionDef _ _ _ _) = True
isFunction _ = False

scanFunctions :: [IAST] -> Compiler ()
scanFunctions [] = return ()
scanFunctions (IAFunctionDef name params _ _ : xs) = do
    s <- get
    let idx = length (functionsTable s)
    let meta = FunctionMeta idx (-1) (length params) -- Address -1 placeholder, will be resolved or we need to use a Label
    -- Wait, FunctionTable needs absolute address.
    -- If we use Labels, we can't put Address in FunctionTable until resolved.
    -- But BytecodeFile needs FunctionTable with addresses.
    -- So we need to resolve Labels BEFORE creating BytecodeFile.
    -- Currently resolveLabels returns [Instruction]. FunctionTable is separate.
    -- We need to update FunctionTable addresses after label resolution.
    
    put $ s { 
        functionsTable = functionsTable s ++ [meta],
        funcMap = Map.insert name idx (funcMap s)
    }
    scanFunctions xs
scanFunctions (_:xs) = scanFunctions xs

emit :: InterInstruction -> Compiler ()
emit instr = modify $ \s -> s { interInstructions = interInstructions s ++ [instr] }

freshLabel :: Compiler String
freshLabel = do
    s <- get
    let l = labelCounter s
    put $ s { labelCounter = l + 1 }
    return ("L" ++ show l)

pushScope :: [String] -> Compiler ()
pushScope vars = modify $ \s -> s { scopeStack = vars : scopeStack s }

popScope :: Compiler ()
popScope = modify $ \s -> s { scopeStack = drop 1 (scopeStack s) }

compile :: IAST -> Compiler ()
compile (IANumber n) = emit $ RealInstr (PushInt (fromIntegral n))
compile (IAFloatLiteral f) = emit $ RealInstr (PushFloat (realToFrac f))
compile (IABoolean b) = emit $ RealInstr (PushBool b)
compile (IAString s) = do
    idx <- getConstIndex (BCString s)
    emit $ RealInstr (PushString idx)

compile (IAInfix left op right) = do
    compile left
    compile right
    case op of
        "+" -> emit $ RealInstr Add
        "-" -> emit $ RealInstr Sub
        "*" -> emit $ RealInstr Mul
        "/" -> emit $ RealInstr Div
        "mod" -> emit $ RealInstr Mod
        "==" -> emit $ RealInstr Eq
        "!=" -> emit $ RealInstr Neq
        "<" -> emit $ RealInstr Lt
        ">" -> emit $ RealInstr Gt
        "<=" -> emit $ RealInstr Le
        ">=" -> emit $ RealInstr Ge
        _ -> error $ "Op " ++ op

compile (IACall "print" args) = do
    mapM_ compile args
    emit $ RealInstr Print

compile (IACall name args) = do
    mapM_ compile args
    s <- get
    case Map.lookup name (funcMap s) of
        Just idx -> emit $ RealInstr (Call idx (length args))
        Nothing -> error $ "Function not found: " ++ name

compile (IAFunctionDef name params _ body) = do
    let label = "FUNC_" ++ name
    emit $ Label label
    
    -- Extract param names
    let paramNames = map fst params
    pushScope paramNames
    
    mapM_ compile body
    emit $ RealInstr Return
    
    popScope

compile (IAReturn expr) = do
    compile expr
    emit $ RealInstr Return

compile (IADeclare name _ expr) = do
    compile expr
    idx <- getConstIndex (BCString name)
    emit $ RealInstr (Define idx)

compile (IAAssign name expr) = do
    compile expr
    idx <- getConstIndex (BCString name)
    emit $ RealInstr (Store idx)

compile (IASymbol name) = do
    s <- get
    case scopeStack s of
        (currentParams : _) -> 
            case elemIndex name currentParams of
                Just k -> do
                    -- Convert param index k to VM stack index
                    -- Args on stack: [ArgN, ..., Arg1, Arg0] -> CurArgs: [ArgN, ..., Arg0]
                    -- Wait, VM `take argCount`.
                    -- If compiler pushes Arg1, then Arg2. Stack: [Arg2, Arg1].
                    -- CurArgs: [Arg2, Arg1].
                    -- Arg1 (k=0) is at index 1.
                    -- Arg2 (k=1) is at index 0.
                    -- Formula: len - 1 - k.
                    -- Except if params list is [p1, p2]. p1 is k=0.
                    let vmIdx = length currentParams - 1 - k
                    emit $ RealInstr (LoadArg vmIdx)
                Nothing -> loadGlobal name
        [] -> loadGlobal name
  where
    loadGlobal n = do
        idx <- getConstIndex (BCString n)
        emit $ RealInstr (Load idx)

compile (IAIf cond thenB elseB) = do
    lElse <- freshLabel
    lEnd <- freshLabel
    compile cond
    emit $ JumpFalseLabel lElse
    compile thenB
    emit $ JumpLabel lEnd
    emit $ Label lElse
    compile elseB
    emit $ Label lEnd

compile (IAConditional cond thenStmts maybeElse) = do
    lElse <- freshLabel
    lEnd <- freshLabel
    compile cond
    emit $ JumpFalseLabel lElse
    mapM_ compile thenStmts
    emit $ JumpLabel lEnd
    emit $ Label lElse
    case maybeElse of
        Just elseStmts -> mapM_ compile elseStmts
        Nothing -> return ()
    emit $ Label lEnd

compile (IAWhile cond body) = do
    lStart <- freshLabel
    lEnd <- freshLabel
    emit $ Label lStart
    compile cond
    emit $ JumpFalseLabel lEnd
    mapM_ compile body
    emit $ JumpLabel lStart
    emit $ Label lEnd

compile (IAFor init cond update body) = do
    lStart <- freshLabel
    lEnd <- freshLabel
    compile init
    emit $ Label lStart
    compile cond
    emit $ JumpFalseLabel lEnd
    mapM_ compile body
    compile update
    emit $ JumpLabel lStart
    emit $ Label lEnd

compile (IAMain body) = mapM_ compile body -- Compiled inline at the end

compile _ = return ()

getConstIndex :: BCValue -> Compiler Int
getConstIndex val = do
    s <- get
    let pool = constantsPool s
    case elemIndex val pool of
        Just idx -> return idx
        Nothing -> do
            let idx = length pool
            put $ s { constantsPool = pool ++ [val] }
            return idx
