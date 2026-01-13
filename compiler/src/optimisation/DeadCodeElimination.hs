{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- DeadCodeElimination
-}

module DeadCodeElimination (eliminateDeadCode) where

import Types

eliminateDeadCode :: AST -> AST
eliminateDeadCode (IAProgram instrs) = IAProgram (filterNotDead (map eliminateDeadCode instrs))
eliminateDeadCode (IAFunctionDef name params ret body) = IAFunctionDef name params ret (filterNotDead (map eliminateDeadCode body))
eliminateDeadCode (IAMain args body) = IAMain args (filterNotDead (map eliminateDeadCode body))
eliminateDeadCode (IABlock instrs) = IABlock (filterNotDead (map eliminateDeadCode instrs))
eliminateDeadCode (IAIf cond thenB elseB) =
    case cond of
        IABoolean True  -> IABlock (filterNotDead (map eliminateDeadCode thenB))
        IABoolean False -> case elseB of
            Just b  -> IABlock (filterNotDead (map eliminateDeadCode b))
            Nothing -> IANumber 0
        _ -> IAIf cond (filterNotDead (map eliminateDeadCode thenB)) (fmap (filterNotDead . map eliminateDeadCode) elseB)
eliminateDeadCode node = node

filterNotDead :: [AST] -> [AST]
filterNotDead [] = []
filterNotDead (x:xs) =
    case x of
        IAReturn val -> [IAReturn val]
        _            -> x : filterNotDead xs