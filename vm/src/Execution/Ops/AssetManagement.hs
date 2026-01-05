{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- Asset-management
-}

module Execution.Ops.AssetManagement where

import Types (Value(..))
import Execution.State (VMState(..))

opPushConst :: Value -> VMState -> Either String VMState
opPushConst val state = Right $ state { stack = val : stack state }

opPushInt :: Int -> VMState -> Either String VMState
opPushInt i state = Right $ state { stack = IntVal i : stack state }

opPushFloat :: Float -> VMState -> Either String VMState
opPushFloat f state = Right $ state { stack = FloatVal f : stack state }

opPushBool :: Bool -> VMState -> Either String VMState
opPushBool b state = Right $ state { stack = BoolVal b : stack state }

opPushString :: String -> VMState -> Either String VMState
opPushString s state = Right $ state { stack = StringVal s : stack state }

opPushNil :: VMState -> Either String VMState
opPushNil state = Right $ state { stack = NilVal : stack state }

opPop :: VMState -> Either String VMState
opPop state =
    case stack state of
        (_ : rest) -> Right $ state { stack = rest }
        []         -> Left "Error: Pop on empty stack"