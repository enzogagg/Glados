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
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (a == b) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (a == b) : rest }
        (IntVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (fromIntegral a == b) : rest }
        (FloatVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (a == fromIntegral b) : rest }
        (a : b : rest) -> Right $ state { stack = BoolVal (a == b) : rest } -- Fallback for other types
        _ -> Left "Error: Eq requires two values on the stack"

opNeq :: VMState -> Either String VMState
opNeq state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (a /= b) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (a /= b) : rest }
        (IntVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (fromIntegral a /= b) : rest }
        (FloatVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (a /= fromIntegral b) : rest }
        (a : b : rest) -> Right $ state { stack = BoolVal (a /= b) : rest }
        _ -> Left "Error: Neq requires two values on the stack"

opLt :: VMState -> Either String VMState
opLt state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (b < a) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (b < a) : rest }
        (IntVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (b < fromIntegral a) : rest }
        (FloatVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (fromIntegral b < a) : rest }
        _ -> Left "Error: Lt requires two numeric values on the stack"

opGt :: VMState -> Either String VMState
opGt state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (b > a) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (b > a) : rest }
        (IntVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (b > fromIntegral a) : rest }
        (FloatVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (fromIntegral b > a) : rest }
        _ -> Left "Error: Gt requires two numeric values on the stack"

opLte :: VMState -> Either String VMState
opLte state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (b <= a) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (b <= a) : rest }
        (IntVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (b <= fromIntegral a) : rest }
        (FloatVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (fromIntegral b <= a) : rest }
        _ -> Left "Error: Lte requires two numeric values on the stack"

opGte :: VMState -> Either String VMState
opGte state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (b >= a) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (b >= a) : rest }
        (IntVal a : FloatVal b : rest) -> Right $ state { stack = BoolVal (b >= fromIntegral a) : rest }
        (FloatVal a : IntVal b : rest) -> Right $ state { stack = BoolVal (fromIntegral b >= a) : rest }
        _ -> Left "Error: Gte requires two numeric values on the stack"
