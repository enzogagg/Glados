{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-2-glados-1
-- File description:
-- Bytecode Header Generation
-}

module Bytecode.Header (
    generateHeader
) where

import Data.Binary.Put
import Data.Word

-- ==========================
-- Génération du Header
-- ==========================

generateHeader :: Put
generateHeader = do
    -- Magic number "CBC\0"
    putWord8 0x43
    putWord8 0x42
    putWord8 0x43
    putWord8 0x00
    -- Version 1.0
    putWord8 0x01
    putWord8 0x00
    -- Flags
    putWord8 0x00
    -- Reserved (3 bytes)
    putWord8 0x00
    putWord8 0x00
    putWord8 0x00
