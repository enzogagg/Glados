{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- Arithmetic-operations
-}

module Execution.Ops.ArithmeticOperations where

import Types (Value(..))
import Execution.State (VMState(..))

opAdd :: VMState -> Either String VMState
opAdd state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = IntVal (a + b) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = FloatVal (a + b) : rest }
        _ -> Left "Error: Add requires two numeric values of the same type on the stack"

opSub :: VMState -> Either String VMState
opSub state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = IntVal (a - b) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = FloatVal (a - b) : rest }
        _ -> Left "Error: Sub requires two numeric values of the same type on the stack"

opMul :: VMState -> Either String VMState
opMul state =
    case stack state of
        (IntVal a : IntVal b : rest) -> Right $ state { stack = IntVal (a * b) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = FloatVal (a * b) : rest }
        _ -> Left "Error: Mul requires two numeric values of the same type on the stack"

opDiv :: VMState -> Either String VMState
opDiv state =
    case stack state of
        (IntVal _ : IntVal 0 : _) -> Left "Error: Division by zero"
        (FloatVal _ : FloatVal 0.0 : _) -> Left "Error: Division by zero"
        (IntVal a : IntVal b : rest) -> Right $ state { stack = IntVal (a `div` b) : rest }
        (FloatVal a : FloatVal b : rest) -> Right $ state { stack = FloatVal (a / b) : rest }
        _ -> Left "Error: Div requires two numeric values of the same type on the stack"

opMod :: VMState -> Either String VMState
opMod state =
    case stack state of
        (IntVal _ : IntVal 0 : _) -> Left "Error: Modulo by zero"
        (IntVal a : IntVal b : rest) -> Right $ state { stack = IntVal (a `mod` b) : rest }
        _ -> Left "Error: Mod requires two numeric values of the same type on the stack"

opNeg :: VMState -> Either String VMState
opNeg state =
    case stack state of
        (IntVal a : rest) -> Right $ state { stack = IntVal (-a) : rest }
        (FloatVal a : rest) -> Right $ state { stack = FloatVal (-a) : rest }
        _ -> Left "Error: Neg requires a numeric value on the stack"
