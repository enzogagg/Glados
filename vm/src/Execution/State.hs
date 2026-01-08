{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- State
-}

module Execution.State (VMState(..), newVMState) where

import Types (Value, Instruction, FunctionMeta)
import qualified Data.Map as Map

-- === VM State ===
-- This struct represents the state of the virtual machine.

data VMState = VMState {
    stack     :: [Value],               -- The stack of the virtual machine. (Store values)
    env       :: Map.Map String Value,  -- The environment of the virtual machine. (Store variables)
    ip        :: Int,                   -- The instruction pointer. (Current instruction)
    callStack :: [(Int, [Value])],      -- The call stack. (Store return addresses and previous arguments)
    curArgs   :: [Value],               -- The current function arguments.
    isRunning :: Bool,                  -- The boolean to know if the virtual machine is running.

    -- === VM Data ===
    instructions :: [Instruction],      -- The instructions of the virtual machine. (Store instructions)
    constants :: [Value],               -- The constants of the virtual machine. (Store constants)
    functions :: [FunctionMeta]        -- The functions of the virtual machine. (Store functions)
} deriving (Show, Eq)

newVMState :: [Instruction] -> [Value] -> [FunctionMeta] -> VMState
newVMState insts consts funcs = VMState {
    stack = [],
    env = Map.empty,
    ip = 0,
    callStack = [],
    curArgs = [],
    isRunning = False,
    instructions = insts,
    constants = consts,
    functions = funcs
}


