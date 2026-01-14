{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-2-glados-1
-- File description:
-- Bytecode Context
-}

module Bytecode.Context (
    BytecodeContext(..),
    emptyContext,
    addConstant,
    CodeGen
) where

import Types
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Word

-- ==========================
-- Types pour la génération
-- ==========================

data BytecodeContext = BytecodeContext
    { constPool :: [ConstantEntry]
    , constMap :: Map.Map String Int
    , funcTable :: [FunctionEntry]
    , funcMap :: Map.Map String Int
    , nextConstIndex :: Int
    , nextFuncIndex :: Int
    , currentFuncParams :: [(String, Int)]
    , variableTypes :: Map.Map String CladType
    , instructionCount :: Int
    } deriving (Show)

emptyContext :: BytecodeContext
emptyContext = BytecodeContext [] Map.empty [] Map.empty 0 0 [] Map.empty 0

type CodeGen = State BytecodeContext (Either String [Word8])

-- ==========================
-- Gestion du Constant Pool
-- ==========================

addConstant :: ConstantEntry -> State BytecodeContext Int
addConstant entry = do
    ctx <- get
    let key = show entry
    case Map.lookup key (constMap ctx) of
        Just idx -> return idx
        Nothing -> do
            let idx = nextConstIndex ctx
            put ctx { constPool = constPool ctx ++ [entry]
                    , constMap = Map.insert key idx (constMap ctx)
                    , nextConstIndex = idx + 1
                    }
            return idx
