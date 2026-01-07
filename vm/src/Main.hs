{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Main
-}

module Main (main) where

import System.Exit (exitWith, exitSuccess, ExitCode(..))
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (runGet)

import Parser
import Types
import Execution.State (newVMState)
import Execution.Loop (execLoop)

testBytecode = BytecodeFile {
    header = Header 0x43424300 1 0,
    constants = [],
    functions = [],
    instructions = [
        PushInt 5,
        PushInt 3,
        Lt,              -- produit BoolVal True (car 3 < 5)
        PushInt 10,
        PushInt 20,
        Eq,              -- produit BoolVal False (car 10 ≠ 20)
        And,             -- True && False = False
        Print,

        PushInt 7,
        PushInt 7,
        Eq,              -- produit BoolVal True
        Not,             -- !True = False
        Print,
        Halt
    ]
}

main :: IO ()
main = do
    -- Mode test : décommenter ces lignes
    let (BytecodeFile _ consts funcs instrs) = testBytecode
    let state = newVMState instrs consts funcs
    execLoop state
    -- input <- BL.getContents
    -- if BL.null input
    --     then do
    --         putStrLn "Error: No input provided"
    --         exitWith (ExitFailure 84)
    --     else do
    --         let (BytecodeFile _ consts funcs instrs) = runGet parseBytecode input
    --         let state = newVMState instrs consts funcs
    --         execLoop state
    --         exitSuccess
