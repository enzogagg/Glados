{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- Input
-}

module Execution.Ops.Input where

import Types (Value(..))
import Execution.State (VMState(..))

opInput :: String -> VMState -> Either String VMState
opInput inputStr state = Right $ state {
    stack = StringVal inputStr : stack state
}
