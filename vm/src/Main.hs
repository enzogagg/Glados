{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Main
-}

module Main (main) where

import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString.Lazy as BL

import Parser
import Data.Binary.Get (runGet)

main :: IO ()
main = do
    input <- BL.getContents
    if BL.null input
        then do
            putStrLn "Error: No input provided"
            exitWith (ExitFailure 84)
        else do
            BL.putStr input
            exitWith (ExitSuccess)
