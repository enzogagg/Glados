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

import Types()
import System.Directory (doesFileExist)
import System.IO()
import Text.Megaparsec (errorBundlePretty)
import ParseToAST (parseAST)
-- import ParseValue (parseValue)

parseContent :: [String] -> IO (Either String ())
parseContent args = do
    result <- parseArgs args
    case result of
        Left err -> return (Left err)
        Right content -> debugParse content

debugParse :: String -> IO (Either String ())
debugParse content =
    case parseAST content of
        Left errBundle ->
            return (Left (errorBundlePretty errBundle))
        Right ast -> do
            putStrLn (show ast)
            return (Right ())

parseArgs :: [String] -> IO (Either String String)
parseArgs args = case args of
    [] -> do
        return (Left "no input file provided")
    [file] -> do
        if getCladExtension file
            then handleInput file
        else return (Left "invalid file extension (expected .clad)")
    _ -> return (Left "wrong number of arguments (expected one .clad file)")

getCladExtension :: String -> Bool
getCladExtension file = case reverse (takeWhile (/= '.') (reverse file)) of
    "clad" -> True
    _     -> False

handleInput :: String -> IO (Either String String)
handleInput input = do
    exists <- doesFileExist input
    if exists
        then do
            content <- readFile input
            return (Right content)
    else return (Left ("file does not exist: " ++ input))
