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
  , RunMode(..)
  ) where

import Types()
import System.Directory (doesFileExist)
import System.IO()
import Text.Megaparsec (errorBundlePretty)
import ParseToAST (parseAST)
import AstToBin (parseBin)
import Visualizer (astToDot)

import Data.List (isSuffixOf)

data RunMode = Compile | Visualize deriving (Show, Eq)

data CompilerArgs = CompilerArgs
  { inputFile :: String
  , outputFile :: String
  , runMode    :: RunMode
  } deriving (Show, Eq)

parseContent :: [String] -> IO (Either String ())
parseContent args = do
    result <- parseArgs args
    case result of
        Left err -> return (Left err)
        Right cArgs -> do
            contentResult <- handleInput (inputFile cArgs)
            case contentResult of
                Left err -> return (Left err)
                Right content -> useContent content cArgs

useContent :: String -> CompilerArgs -> IO (Either String ())
useContent content cArgs =
    case parseAST content of
        Left errBundle ->
            return (Left (errorBundlePretty errBundle))
        Right ast ->
            case runMode cArgs of
                Visualize -> do
                    writeFile "ast.dot" (astToDot ast)
                    putStrLn "Fichier 'ast.dot' généré.\ndot -Tpng ast.dot -o ast.png pour visualiser."
                    return (Right ())
                Compile -> do
                    parseBin ast (outputFile cArgs)
                    return (Right ())

parseArgs :: [String] -> IO (Either String CompilerArgs)
parseArgs args = return $ parseArgsInternal args Nothing Compile
  where
    parseArgsInternal :: [String] -> Maybe String -> RunMode -> Either String CompilerArgs

    parseArgsInternal ("-o":outName:rest) _ mode = parseArgsInternal rest (Just outName) mode

    parseArgsInternal ("--visualize":rest) out _ = parseArgsInternal rest out Visualize

    parseArgsInternal [file] out mode =
        if getCladExtension file
            then Right $ CompilerArgs file (ensureCbc (maybe "a.out.cbc" id out)) mode
            else Left "Erreur : Extension de fichier invalide (attendu .clad)"

    parseArgsInternal _ _ _ = Left "USAGE\n    ./glados-compiler [-o <out.cbc>] [--visualize] <file.clad>"

    ensureCbc name = if ".cbc" `isSuffixOf` name then name else name ++ ".cbc"

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
