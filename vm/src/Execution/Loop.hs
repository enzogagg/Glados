{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- ExecutionLoop
-}

module Execution.Loop where

import Execution.State (VMState(..))
import Types (Instruction(..), Value(..))
import Execution.Ops (opPushConst, opPushInt, opPop, opAdd, opSub, opEq)

execLoop :: VMState -> IO ()
execLoop state =
    if ip state >= length (instructions state) || ip state < 0
    then putStrLn "Error: Instruction pointer out of bounds"
    else case instructions state !! ip state of
        Halt -> return ()
        PushConst idx -> nextStep (opPushConst (constants state !! idx) state)
        PushInt i -> nextStep (opPushInt i state)
        Pop -> nextStep (opPop state)
        Add -> nextStep (opAdd state)
        Sub -> nextStep (opSub state)
        Eq -> nextStep (opEq state)
        Print -> case stack state of
                    [] -> putStrLn "Error: Print requires a value on the stack"
                    (val : _) -> do
                        print val
                        nextStep (Right (state {ip = ip state}))
        _ -> nextStep (Left "Error: Unknown instruction")
    where
        nextStep :: Either String VMState -> IO ()
        nextStep (Left err) = putStrLn ("Runtime Error: " ++ err)
        nextStep (Right newState) = execLoop (newState {ip = ip newState + 1})
