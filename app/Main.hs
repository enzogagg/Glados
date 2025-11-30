{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import ParseArguments (parseContent)

main :: IO ()
main = do
    args <- getArgs
    result <- parseContent args
    case result of
        Right () -> return ()
        Left err -> do
            putStrLn ("*** ERROR : " ++ err)
            exitWith (ExitFailure 84)
