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

main :: IO ()
main = do
    input <- BL.getContents
    if BL.null input
        then do
            putStrLn "Error: No input provided"
            exitWith (ExitFailure 84)
        else do
            let (BytecodeFile _ consts funcs instrs) = runGet parseBytecode input
            let state = newVMState instrs consts funcs
            execLoop state
            exitSuccess
