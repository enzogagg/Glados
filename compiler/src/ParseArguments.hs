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
  , useContent
  , CompilerArgs(..)
  ) where

import Types()
import System.Directory (doesFileExist)
import System.IO()
import Text.Megaparsec (errorBundlePretty)
import ParseToAST (parseAST)
import AstToBin (parseBin)

data CompilerArgs = CompilerArgs
  { inputFile :: String
  , outputFile :: String
  } deriving (Show, Eq)

parseContent :: [String] -> IO (Either String ())
parseContent args = do
    result <- parseArgs args
    case result of
        Left err -> return (Left err)
        Right compilerArgs -> do
            contentResult <- handleInput (inputFile compilerArgs)
            case contentResult of
                Left err -> return (Left err)
                Right content -> useContent content (outputFile compilerArgs)

useContent :: String -> String -> IO (Either String ())
useContent content outputName =
    case parseAST content of
        Left errBundle ->
            return (Left (errorBundlePretty errBundle))
        Right ast -> do
            -- print ast
            result <- parseBin ast outputName
            return result

parseArgs :: [String] -> IO (Either String CompilerArgs)
parseArgs args = return $ parseArgsInternal args Nothing
  where
    parseArgsInternal :: [String] -> Maybe String -> Either String CompilerArgs
    parseArgsInternal [] _ = Left "USAGE\n    ./glados-compiler (-h | <file_input.clad> [-o <file_output.cbc>])"
    parseArgsInternal ["-h"] _ = Left "USAGE\n    ./glados-compiler (-h | <file_input.clad> [-o <file_output.cbc>])"
    parseArgsInternal [file] Nothing =
        if getCladExtension file
            then Right $ CompilerArgs file (ensureCbcExtension "a.out.cbc")
            else Left "invalid file extension (expected .clad)"
    parseArgsInternal [file] (Just output) =
        if getCladExtension file
            then Right $ CompilerArgs file (ensureCbcExtension output)
            else Left "invalid file extension (expected .clad)"
    parseArgsInternal (file:"-o":outName:rest) _ =
        if getCladExtension file && null rest
            then Right $ CompilerArgs file (ensureCbcExtension outName)
            else Left "USAGE\n    ./glados-compiler (-h | <file_input.clad> [-o <file_output.cbc>])"
    parseArgsInternal _ _ = Left "USAGE\n    ./glados-compiler (-h | <file_input.clad> [-o <file_output.cbc>])"

    ensureCbcExtension :: String -> String
    ensureCbcExtension name =
        if ".cbc" `isSuffixOf` name
            then name
            else name ++ ".cbc"

    isSuffixOf :: String -> String -> Bool
    isSuffixOf suffix str = suffix == reverse (take (length suffix) (reverse str))

getCladExtension :: String -> Bool
getCladExtension file =
    let ext = reverse (takeWhile (/= '.') (reverse file))
    in ext == "clad"

handleInput :: String -> IO (Either String String)
handleInput input = do
    exists <- doesFileExist input
    if exists
        then do
            content <- readFile input
            return (Right content)
    else return (Left ("file does not exist: " ++ input))