{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- List-operations
-}

module Execution.Ops.ListOperations where

import Types (Value(..))
import Execution.State (VMState(..))

opCons :: VMState -> Either String VMState
opCons state =
    case stack state of
        (v1:v2:rest) -> case v2 of
            ListVal lst -> Right $ state {stack = ListVal (v1:lst) : rest}
            NilVal -> Right $ state {stack = ListVal [v1] : rest}
            _ -> Left "Error: Cons requires second value to be a list"
        _ -> Left "Error: Cons requires two values on the stack"

opHead :: VMState -> Either String VMState
opHead state =
    case stack state of
        (v:rest) -> case v of
            ListVal (x:_) -> Right $ state {stack = x : rest}
            ListVal [] -> Left "Error: Head on empty list"
            _ -> Left "Error: Head requires a list on the stack"
        _ -> Left "Error: Head requires a value on the stack"

opTail :: VMState -> Either String VMState
opTail state =
    case stack state of
        (v:rest) -> case v of
            ListVal (_:xs) -> Right $ state {stack = ListVal xs : rest}
            ListVal [] -> Left "Error: Tail on empty list"
            _ -> Left "Error: Tail requires a list on the stack"
        _ -> Left "Error: Tail requires a value on the stack"

opList :: Int -> VMState -> Either String VMState
opList n state =
    if length (stack state) < n
    then Left "Error: Not enough values on the stack to create list"
    else let (vals, rest) = splitAt n (stack state)
         in Right $ state {stack = ListVal (reverse vals) : rest}

opLen :: VMState -> Either String VMState
opLen state =
    case stack state of
        (v:rest) -> case v of
            ListVal lst -> Right $ state {stack = IntVal (length lst) : rest}
            _ -> Left "Error: Len requires a list on the stack"
        _ -> Left "Error: Len requires a value on the stack"
