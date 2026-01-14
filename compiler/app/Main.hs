{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Main
-}

module Main (main) where

import ParseArguments (parseContent)
import Interpreter (runREPL)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runREPL
        _  -> do
            result <- parseContent args
            case result of
                Left err -> putStrLn err >> exitFailure
                Right _  -> return ()