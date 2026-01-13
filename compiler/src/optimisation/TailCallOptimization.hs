{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- TailCallOptimization
-}

module TailCallOptimization (optimizeTailCalls) where

import Types

optimizeTailCalls :: AST -> AST
optimizeTailCalls (IAProgram instrs) = IAProgram (map optimizeTailCalls instrs)
optimizeTailCalls (IAFunctionDef name params ret body) = 
    IAFunctionDef name params ret (markLastInstruction body)
optimizeTailCalls (IAMain args body) = IAMain args (markLastInstruction body)
optimizeTailCalls node = node

-- Fonction qui cherche la dernière instruction pour y trouver un appel
markLastInstruction :: [AST] -> [AST]
markLastInstruction [] = []
markLastInstruction [IAReturn (IACall name args)] = [IAReturn (IATailCall name args)]
markLastInstruction [IAIf cond thenB elseB] = 
    [IAIf cond (markLastInstruction thenB) (fmap markLastInstruction elseB)]
markLastInstruction (x:xs) = x : markLastInstruction xs