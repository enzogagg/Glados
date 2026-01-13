module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (runGetOrFail)
import Control.Exception (catch, IOException)

import Parser
import Types
import Execution.State (newVMState)
import Execution.Loop (execLoop)

main :: IO ()
main = do
    args <- getArgs
    (input, vmArgs) <- case args of
        ("-h":_) -> printHelp >> exitSuccess
        [] -> do
            c <- BL.getContents
            return (c, [])
        (file:rest) -> do
            c <- BL.readFile file
            return (c, args)
            
    if BL.null input
        then do
            putStrLn "Error: No input provided"
            exitWith (ExitFailure 84)
        else do
            case runGetOrFail parseBytecode input of
                Left (_, _, err) -> do
                    putStrLn $ "Error parsing bytecode: " ++ err
                    exitWith (ExitFailure 84)
                Right (_, _, BytecodeFile _ consts funcs instrs) -> do
                    let state = newVMState instrs consts funcs vmArgs
                    execLoop state
                    exitSuccess

printHelp :: IO ()
printHelp = do
    putStrLn "USAGE: ./glados-vm [file.cbc]"
    putStrLn "       file.cbc   file to execute"
