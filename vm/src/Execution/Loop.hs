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
import Execution.Ops.ComparisonOperations(opEq, opNeq, opLt, opGt, opLte, opGte)
import Execution.Ops.ListOperations (opCons, opHead, opTail, opList, opLen)
import Execution.Ops.SymbolsAndEvaluation(opMakeSymbol, opQuote, opEval)
import Execution.Ops.VariableManagement(opLoad, opStore, opDefine)
import Execution.Ops.FlowControl (opJump, opJumpIfTrue, opJumpIfFalse)
import Execution.Ops.FunctionManagement (opCall, opReturn, opClosure, opLoadArg)
import Execution.Ops.Input (opInput)

execLoop :: VMState -> IO ()
execLoop state =
    if ip state >= length (instructions state) || ip state < 0
    then putStrLn "Error: Instruction pointer out of bounds"
    else case instructions state !! ip state of
        Halt -> return ()
        PushConst idx -> nextStep (opPushConst (constants state !! idx) state)
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
        Cons -> nextStep (opCons state)
        Head -> nextStep (opHead state)
        Tail -> nextStep (opTail state)
        ListMake n -> nextStep (opList n state)
        Len -> nextStep (opLen state)
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
                    [] -> putStrLn "Error: Print requires a value on the stack"
                    (val : _) -> do
                        print val
                        nextStep (Right (state {ip = ip state}))
        Input -> do
            line <- getLine
            nextStep (opInput line state)
        _ -> nextStep (Left "Error: Unknown instruction")
    where
        nextStep :: Either String VMState -> IO ()
        nextStep (Left err) = putStrLn ("Runtime Error: " ++ err)
        nextStep (Right newState) = execLoop (newState {ip = ip newState + 1})
