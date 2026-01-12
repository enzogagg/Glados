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

opIsEmpty :: VMState -> Either String VMState
opIsEmpty state =
    case stack state of
        (v:rest) -> case v of
            ListVal lst -> Right $ state {stack = BoolVal (null lst) : rest}
            _ -> Left "Error: IsEmpty requires a list on the stack"
        _ -> Left "Error: IsEmpty requires a value on the stack"

opNth :: VMState -> Either String VMState
opNth state =
    case stack state of
        (IntVal n : ListVal lst : rest) ->
            if n < 0 || n >= length lst
            then Left "Error: Nth index out of bounds"
            else Right $ state {stack = lst !! n : rest}
        _ -> Left "Error: Nth requires an integer and a list on the stack"

opInsert :: VMState -> Either String VMState
opInsert state =
    case stack state of
        (v : IntVal n : ListVal lst : rest) ->
            if n < 0 || n > length lst
            then Left "Error: Insert index out of bounds"
            else let (front, back) = splitAt n lst
                 in Right $ state {stack = ListVal (front ++ (v : back)) : rest}
        _ -> Left "Error: Insert requires a value, an integer, and a list on the stack"

opRemove :: VMState -> Either String VMState
opRemove state =
    case stack state of
        (IntVal n : ListVal lst : rest) ->
            if n < 0 || n >= length lst
            then Left "Error: Remove index out of bounds"
            else let (front, _:back) = splitAt n lst
                 in Right $ state {stack = ListVal (front ++ back) : rest}
        _ -> Left "Error: Remove requires an integer and a list on the stack"

opContains :: VMState -> Either String VMState
opContains state =
    case stack state of
        (v : ListVal lst : rest) ->
            Right $ state {stack = BoolVal (v `elem` lst) : rest}
        _ -> Left "Error: Contains requires a value and a list on the stack"

opAppend :: VMState -> Either String VMState
opAppend state =
    case stack state of
        (ListVal lst2 : ListVal lst1 : rest) ->
            Right $ state {stack = ListVal (lst1 ++ lst2) : rest}
        _ -> Left "Error: Append requires two lists on the stack"

opReverse :: VMState -> Either String VMState
opReverse state =
    case stack state of
        (ListVal lst : rest) ->
            Right $ state {stack = ListVal (reverse lst) : rest}
        _ -> Left "Error: Reverse requires a list on the stack"