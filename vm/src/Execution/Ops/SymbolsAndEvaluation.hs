{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- Symbols-and-evaluation
-}

module Execution.Ops.SymbolsAndEvaluation where

import Types (Value(..))
import Execution.State (VMState(..))

opMakeSymbol :: VMState -> Either String VMState
opMakeSymbol state =
    case stack state of
        (StringVal s : rest) -> Right $ state {stack = SymbolVal s : rest}
        _ -> Left "Error: MakeSymbol requires a string on the stack"

opQuote :: VMState -> Either String VMState
opQuote state =
    case stack state of
        (StringVal s : rest) -> Right $ state {stack = SymbolVal s : rest}
        _ -> Left "Error: Quote requires a string on the stack"

opEval :: VMState -> Either String VMState
opEval state =
    case stack state of
        (StringVal s : rest) -> Right $ state {stack = SymbolVal s : rest}
        _ -> Left "Error: Eval requires a string on the stack"
