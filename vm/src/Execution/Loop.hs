{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- ExecutionLoop
-}

module Execution.Loop where

import Execution.State (VMState(..))
import Types (Instruction(..), Value(..))
import Execution.Ops.ArithmeticOperations(opAdd, opSub, opMul, opDiv, opMod, opNeg)
import Execution.Ops.AssetManagement(opPushConst, opPushInt, opPushFloat, opPushBool, opPushString, opPushNil, opPop)
import Execution.Ops.ComparisonOperations(opEq, opNeq, opLt, opGt, opLte, opGte, opAnd, opNot, opOr)
import Execution.Ops.ListOperations (opCons, opHead, opTail, opList, opLen, opIsEmpty, opNth, opInsert, opRemove, opContains, opAppend, opReverse)
import Execution.Ops.SymbolsAndEvaluation(opMakeSymbol, opQuote, opEval)
import Execution.Ops.VariableManagement(opLoad, opStore, opDefine)
import Execution.Ops.FlowControl (opJump, opJumpIfTrue, opJumpIfFalse)
import Execution.Ops.FunctionManagement (opCall, opReturn, opClosure, opLoadArg)
import Execution.Ops.Input (opInput)
import Execution.Ops.ComplexDataStructures (opMakeTuple, opTupleGet, opMakeArray, opArrayGet, opArraySet, opMakeMap, opMapGet, opMapSet, opMakeStruct, opStructGet, opStructSet)
import Execution.Ops.FileManagement (opOpenFile, opReadFile, opWriteFile, opCloseFile)

execLoop :: VMState -> IO (Maybe Int)
execLoop state =
    if ip state == length (instructions state)
    then case stack state of
        (IntVal n : _) -> return (Just n)
        _ -> return Nothing
    else if ip state > length (instructions state) || ip state < 0
    then putStrLn "Error: Instruction pointer out of bounds" >> return Nothing
    else case instructions state !! ip state of
        Halt -> case stack state of
            (IntVal n : _) -> return (Just n)
            _ -> return Nothing
        PushConst idx ->
            if idx < 0 || idx >= length (constants state)
            then nextStep (Left ("Error: Constant index out of bounds: " ++ show idx))
            else nextStep (opPushConst (constants state !! idx) state)
        PushInt i -> nextStep (opPushInt i state)
        PushFloat f -> nextStep (opPushFloat f state)
        PushBool b -> nextStep (opPushBool b state)
        PushString s -> nextStep (opPushString s state)
        PushNil -> nextStep (opPushNil state)
        Pop -> nextStep (opPop state)

        Add -> nextStep (opAdd state)
        Sub -> nextStep (opSub state)
        Mul -> nextStep (opMul state)
        Div -> nextStep (opDiv state)
        Mod -> nextStep (opMod state)
        Neg -> nextStep (opNeg state)

        Eq -> nextStep (opEq state)
        Neq -> nextStep (opNeq state)
        Lt -> nextStep (opLt state)
        Gt -> nextStep (opGt state)
        Le -> nextStep (opLte state)
        Ge -> nextStep (opGte state)
        And -> nextStep (opAnd state)
        Or -> nextStep (opOr state)
        Not -> nextStep (opNot state)
        Cons -> nextStep (opCons state)
        Head -> nextStep (opHead state)
        Tail -> nextStep (opTail state)
        ListMake n -> nextStep (opList n state)
        Len -> nextStep (opLen state)
        IsEmpty -> nextStep (opIsEmpty state)
        Nth -> nextStep (opNth state)
        Insert -> nextStep (opInsert state)
        Remove -> nextStep (opRemove state)
        Contains -> nextStep (opContains state)
        Append -> nextStep (opAppend state)
        Reverse -> nextStep (opReverse state)

        MakeSymbol -> nextStep (opMakeSymbol state)
        Quote -> nextStep (opQuote state)
        Eval -> nextStep (opEval state)

        Load s -> nextStep (opLoad s state)
        Store s -> nextStep (opStore s state)
        Define s -> nextStep (opDefine s state)

        Jump addr -> nextStep (opJump addr state)
        JumpIfTrue addr -> nextStep (opJumpIfTrue addr state)
        JumpIfFalse addr -> nextStep (opJumpIfFalse addr state)

        Call funcId argCount -> nextStep (opCall funcId argCount state)
        Return -> nextStep (opReturn state)
        Closure funcId -> nextStep (opClosure funcId state)
        LoadArg idx -> nextStep (opLoadArg idx state)

        Print -> case stack state of
                    [] -> putStrLn "Error: Print requires a value on the stack" >> return Nothing
                    (val : _) -> do
                        print val
                        nextStep (Right (state {ip = ip state}))
        Input -> do
            line <- getLine
            nextStep (opInput line state)
        MakeTuple n -> nextStep (opMakeTuple n state)
        TupleGet idx -> nextStep (opTupleGet idx state)
        MakeArray n -> nextStep (opMakeArray n state)
        ArrayGet -> nextStep (opArrayGet state)
        ArraySet -> nextStep (opArraySet state)
        MakeMap n -> nextStep (opMakeMap n state)
        MapGet -> nextStep (opMapGet state)
        MapSet -> nextStep (opMapSet state)
        MakeStruct n -> nextStep (opMakeStruct n state)
        StructGet idx -> nextStep (opStructGet idx state)
        StructSet idx -> nextStep (opStructSet idx state)

        OpenFile -> nextStepIO (opOpenFile state)
        ReadFile -> nextStepIO (opReadFile state)
        WriteFile -> nextStepIO (opWriteFile state)
        CloseFile -> nextStepIO (opCloseFile state)

        _ -> nextStep (Left "Error: Unknown instruction")
    where
        nextStep :: Either String VMState -> IO (Maybe Int)
        nextStep (Left err) = putStrLn ("Runtime Error: " ++ err) >> return Nothing
        nextStep (Right newState) = execLoop (newState {ip = ip newState + 1})

        nextStepIO :: IO (Either String VMState) -> IO (Maybe Int)
        nextStepIO action = do
            result <- action
            nextStep result
