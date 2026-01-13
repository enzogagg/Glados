module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (runGetOrFail)
import Control.Exception (catch, IOException)
import Data.List (partition)
import Control.Monad (when)

import Parser
import Types
import Execution.State (newVMState)
import Execution.Loop (execLoop)
import Disassembler (disassemble)

main :: IO ()
main = do
    args <- getArgs
    let (flags, regularArgs) = partition (== "-d") args
    let debug = not (null flags)

    (input, vmArgs) <- case regularArgs of
        ("-h":_) -> printHelp >> exitSuccess
        [] -> do
            c <- BL.getContents
            return (c, [])
        (file:rest) -> do
            c <- BL.readFile file
            return (c, args) -- Pass original args including flags for VM argv, or regularArgs?
            -- Usually VM argv shouldn't contain VM flags. Let's pass regularArgs (minus -d)
            -- But wait, standard practice is argv[0] is program name.
            -- If user does `./vm prog -d arg1`, regularArgs is `["prog", "arg1"]`.
            -- Returning `regularArgs` seems correct to clean `-d`.
            
    if BL.null input
        then do
            putStrLn "Error: No input provided"
            exitWith (ExitFailure 84)
        else do
            case runGetOrFail parseBytecode input of
                Left (_, _, err) -> do
                    putStrLn $ "Error parsing bytecode: " ++ err
                    exitWith (ExitFailure 84)
                Right (_, _, bytecode@(BytecodeFile _ consts funcs instrs)) -> do
                    when debug $ disassemble bytecode
                    -- Use regularArgs to avoid passing -d to the program
                    let state = newVMState instrs consts funcs regularArgs
                    execLoop state
                    exitSuccess

printHelp :: IO ()
printHelp = do
    putStrLn "USAGE: ./glados-vm [options] [file.cbc]"
    putStrLn "       file.cbc   file to execute"
    putStrLn "       -d         print disassembly before execution"
