module Execution.Ops.FunctionManagement where

import Types (Value(..), FunctionMeta(..))
import Execution.State (VMState(..))
import Data.List (find)

-- Helper to find function by ID
findFunction :: Int -> [FunctionMeta] -> Maybe FunctionMeta
findFunction fid funcs = find (\f -> funcId f == fid) funcs

opCall :: Int -> Int -> VMState -> Either String VMState
opCall funcIndex argCount state =
    case findFunction funcIndex (functions state) of
        Nothing -> Left $ "Function not found: " ++ show funcIndex
        Just func ->
            if length (stack state) < argCount
            then Left "Insufficient arguments on stack"
            else
                let args = take argCount (stack state)
                    remainingStack = drop argCount (stack state)
                    savedIp = ip state
                    savedArgs = curArgs state
                    newCallStack = (savedIp, savedArgs) : callStack state
                in Right $ state {
                    stack = remainingStack,
                    ip = funcAddress func - 1, -- Decrement by 1 because loop increments
                    curArgs = args,
                    callStack = newCallStack
                }

opReturn :: VMState -> Either String VMState
opReturn state =
    case callStack state of
        [] -> Left "Return outside of function call"
        ((retIp, oldArgs) : rest) ->
            Right $ state {
                ip = retIp,
                curArgs = oldArgs,
                callStack = rest
            }

opClosure :: Int -> VMState -> Either String VMState
opClosure funcIndex state =
    Right $ state {
        stack = FunctionVal funcIndex : stack state
    }

opLoadArg :: Int -> VMState -> Either String VMState
opLoadArg index state =
    let args = curArgs state
    in if index < 0 || index >= length args
       then Left $ "Argument index out of bounds: " ++ show index
       else Right $ state {
           stack = (args !! index) : stack state
       }