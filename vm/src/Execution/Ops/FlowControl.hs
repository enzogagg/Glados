{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- Flow-control
-}

module Execution.Ops.FlowControl where

import Types (Value(..))
import Execution.State (VMState(..))

opJump :: Int -> VMState -> Either String VMState
opJump addr state = Right $ state { ip = addr - 1 }

opJumpIfTrue :: Int -> VMState -> Either String VMState
opJumpIfTrue addr state =
    case stack state of
        (BoolVal True : rest) -> Right $ state { stack = rest, ip = addr - 1 }
        (BoolVal False : rest) -> Right $ state { stack = rest }
        (_ : _) -> Left "Error: JumpIfTrue requires a boolean on the stack"
        [] -> Left "Error: JumpIfTrue requires a value on the stack"

opJumpIfFalse :: Int -> VMState -> Either String VMState
opJumpIfFalse addr state =
    case stack state of
        (BoolVal False : rest) -> Right $ state { stack = rest, ip = addr - 1 }
        (BoolVal True : rest) -> Right $ state { stack = rest }
        (_ : _) -> Left "Error: JumpIfFalse requires a boolean on the stack"
        [] -> Left "Error: JumpIfFalse requires a value on the stack"