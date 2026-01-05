{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- Comparison-operations
-}

module Execution.Ops.ComparisonOperations where

import Types (Value(..))
import Execution.State (VMState(..))

opEq :: VMState -> Either String VMState
opEq state =
    case stack state of
        (a : b : rest) -> Right $ state { stack = BoolVal (a == b) : rest }
        _ -> Left "Error: Eq requires two values on the stack"

opNeq :: VMState -> Either String VMState
opNeq state =
    case stack state of
        (a : b : rest) -> Right $ state { stack = BoolVal (a /= b) : rest }
        _ -> Left "Error: Neq requires two values on the stack"

opLt :: VMState -> Either String VMState
opLt state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (b < a) : rest }
        _ -> Left "Error: Lt requires two integer values on the stack"

opGt :: VMState -> Either String VMState
opGt state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (b > a) : rest }
        _ -> Left "Error: Gt requires two integer values on the stack"

opLte :: VMState -> Either String VMState
opLte state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (b <= a) : rest }
        _ -> Left "Error: Le requires two integer values on the stack"

opGte :: VMState -> Either String VMState
opGte state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (b >= a) : rest }
        _ -> Left "Error: Ge requires two integer values on the stack"
