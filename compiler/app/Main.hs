module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import ParseToAST (parseAST)
import Compiler (compileProgram)
import BytecodeGenerator (encodeBytecodeFile)
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> do
            content <- readFile inputFile
            case parseAST content of
                Left err -> do
                    print err
                    exitWith (ExitFailure 84)
                Right ast -> do
                    let bytecode = compileProgram ast
                    let binary = encodeBytecodeFile bytecode
                    -- Default output: replace extension or out.cbc
                    let outputFile = "out.cbc" 
                    BSL.writeFile outputFile binary
                    putStrLn $ "Compiled " ++ inputFile ++ " to " ++ outputFile
        _ -> do
            putStrLn "Usage: glados-compiler <file.clad>"
            exitWith (ExitFailure 84)
