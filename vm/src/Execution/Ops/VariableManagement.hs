module Execution.Ops.VariableManagement where

import Types (Value(..))
import Execution.State (VMState(..))
import qualified Data.Map as Map

opLoad :: String -> VMState -> Either String VMState
opLoad name state =
    case Map.lookup name (env state) of
        Just val -> Right $ state { stack = val : stack state }
        Nothing  -> Left $ "Error: Variable '" ++ name ++ "' not defined"

opStore :: String -> VMState -> Either String VMState
opStore name state =
    case stack state of
        (val : rest) ->
            if Map.member name (env state)
            then Right $ state { stack = rest, env = Map.insert name val (env state) }
            else Left $ "Error: Variable '" ++ name ++ "' not defined (use Define first)"
        [] -> Left "Error: Store requires a value on the stack"

opDefine :: String -> VMState -> Either String VMState
opDefine name state =
    case stack state of
        (val : rest) -> Right $ state { stack = rest, env = Map.insert name val (env state) }
        [] -> Left "Error: Define requires a value on the stack"