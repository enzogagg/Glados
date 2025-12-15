{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- Ops
-}

module Execution.Ops where

import Types (Value(..))
import Execution.State (VMState(..))

opPushConst :: Value -> VMState -> Either String VMState
opPushConst val state = Right $ state { stack = val : stack state }

opPushInt :: Int -> VMState -> Either String VMState
opPushInt i state = Right $ state { stack = IntVal i : stack state }

opPop :: VMState -> Either String VMState
opPop state =
    case stack state of
        (_ : rest) -> Right $ state { stack = rest }
        []         -> Left "Error: Pop on empty stack"

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

opEq :: VMState -> Either String VMState
opEq state =
    case stack state of
        (a : b : rest) -> Right $ state { stack = BoolVal (a == b) : rest }
        _ -> Left "Error: Eq requires two values on the stack"
