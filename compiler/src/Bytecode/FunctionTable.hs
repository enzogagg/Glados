{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-2-glados-1
-- File description:
-- Bytecode Function Table Generation
-}

module Bytecode.FunctionTable (
    putFunctionEntry,
    generateFunctionTable
) where

import Types
import Data.Binary.Put
import Data.Word()

-- ==========================
-- Génération de la Function Table
-- ==========================

putFunctionEntry :: FunctionEntry -> Put
putFunctionEntry fe = do
    putWord32be (fromIntegral $ funcIndex fe)
    putWord32be (fromIntegral $ funcAddress fe)
    putWord8 (fromIntegral $ funcArgCount fe)

generateFunctionTable :: [FunctionEntry] -> Put
generateFunctionTable entries = do
    putWord32be (fromIntegral $ length entries)
    mapM_ putFunctionEntry entries
