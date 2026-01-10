{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import ParseToAST (parseAST)
import Text.Megaparsec (errorBundlePretty) 
import Interpreter (runREPL)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runREPL
        [inputFile] -> do
            content <- readFile inputFile
            case parseAST content of
                Left err -> putStrLn (errorBundlePretty err) >> exitFailure 
                Right ast -> putStrLn "Compilation réussie (AST généré)."
        _ -> putStrLn "Usage: ./glados-compiler [file.clad]"