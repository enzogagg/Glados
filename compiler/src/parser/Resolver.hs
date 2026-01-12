module Resolver (resolveIncludes) where

import Types
import ParseToAST (parseAST)
import System.Directory (doesFileExist)
import Control.Monad (foldM)

resolveIncludes :: AST -> IO (Either String AST)
resolveIncludes ast = resolve ast []

resolve :: AST -> [FilePath] -> IO (Either String AST)
resolve (IAProgram instrs) visited = do
    res <- foldM (processNode visited) (Right []) instrs
    return $ IAProgram <$> res
resolve other _ = return $ Right other

processNode :: [FilePath] -> Either String [AST] -> AST -> IO (Either String [AST])
processNode _ (Left err) _ = return $ Left err
processNode visited (Right acc) (IAInclude path)
    | path `elem` visited = return $ Left ("Boucle d'inclusion détectée : " ++ path)
    | otherwise = do
        exists <- doesFileExist path
        if not exists
            then return $ Left ("Fichier introuvable : " ++ path)
            else do
                content <- readFile path
                case parseAST content of
                    Left err -> return $ Left ("Erreur dans " ++ path ++ ": " ++ show err)
                    Right (IAProgram newInstrs) -> do
                        subRes <- resolve (IAProgram newInstrs) (path : visited)
                        case subRes of
                            Left err -> return $ Left err
                            Right (IAProgram resolved) -> return $ Right (acc ++ resolved)
                            Right otherNode -> return $ Right (acc ++ [otherNode])
                    Right node -> return $ Right (acc ++ [node])
processNode _ (Right acc) node = return $ Right (acc ++ [node])