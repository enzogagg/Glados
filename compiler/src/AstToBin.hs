{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-2-glados-1
-- File description:
-- AstToBin - Main Bytecode Generation Orchestrator
-}

module AstToBin (
    parseBin,
) where

import Types
import Bytecode.Context
import Bytecode.Header
import Bytecode.ConstantPool
import Bytecode.FunctionTable
import Bytecode.Instructions

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Control.Monad.State

-- ==========================
-- Fonction principale
-- ==========================

parseBin :: AST -> String -> IO (Either String ())
parseBin ast outputName = do
    let (instructionsResult, finalCtx) = runState (generateInstruction ast) emptyContext

    case instructionsResult of
        Left err -> return $ Left $ "Bytecode generation error: " ++ err
        Right instructions -> do
            let bytecode = runPut $ do
                    generateHeader
                    generateConstantPool (constPool finalCtx)
                    generateFunctionTable (funcTable finalCtx)
                    putWord32be (fromIntegral $ length instructions)
                    mapM_ putWord8 instructions

            BS.writeFile outputName (BL.toStrict bytecode)
            return $ Right ()