{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ParseArguments
-}

module ParseArguments
  ( parseContent
  , parseArgs
  , handleInput
  , getCladExtension
  , debugParse
  ) where

import Types
import System.Directory (doesFileExist)
import System.IO (stdin, hIsTerminalDevice, isEOF, hFlush, stdout)
import Text.Megaparsec (parse, errorBundlePretty)
import ParseToExpr (parseProgram)
import ParseValue (parseValue, runExprs, builtins)

parseContent :: [String] -> IO (Either String ())
parseContent args = do
    result <- parseArgs args
    case result of
        Left err -> return (Left err)
        Right "__REPL__" -> startRepl
        Right content -> debugParse content

debugParse :: String -> IO (Either String ())
debugParse content =
    case parse parseProgram "" content of
        Left errBundle ->
            return (Left (errorBundlePretty errBundle))
        Right exprs ->
            parseValue exprs

parseArgs :: [String] -> IO (Either String String)
parseArgs args = case args of
    [] -> do
        isTerm <- hIsTerminalDevice stdin
        if isTerm
            then return (Right "__REPL__")
        else do
            content <- getContents
            if null content
                then return (Left "empty stdin")
            else return (Right content)
    [file] -> do
        if getCladExtension file
            then handleInput file

        else return (Left "invalid type file")
    _ -> return (Left "wrong number of arguments")

getCladExtension :: String -> Bool
getCladExtension file = case reverse (takeWhile (/= '.') (reverse file)) of
    "clad" -> True
    _      -> False

handleInput :: String -> IO (Either String String)
handleInput input = do
    exists <- doesFileExist input
    if exists
        then do
            content <- readFile input
            return (Right content)
    else return (Left ("file does not exist: " ++ input))

-- =========================
-- REPL MODE
-- =========================

startRepl :: IO (Either String ())
startRepl = do
    putStrLn "Glados REPL. Press Ctrl+D to exit."
    replLoop builtins

replLoop :: Env -> IO (Either String ())
replLoop env = do
    putStr "> "
    hFlush stdout
    eof <- isEOF
    if eof
        then return (Right ())
    else do
        line <- getLine
        case parse parseProgram "" line of
            Left errBundle -> do
                putStr "*** ERROR : "
                putStrLn (errorBundlePretty errBundle)
                replLoop env
            Right exprs -> do
                result <- runExprs env exprs
                case result of
                    Left err -> do
                        putStrLn ("*** ERROR : " ++ err)
                        replLoop env
                    Right newEnv ->
                        replLoop newEnv
