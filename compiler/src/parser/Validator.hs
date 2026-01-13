module Validator (validateAST) where

import Types
import Data.List (nub, (\\))

validateAST :: AST -> Either String ()
validateAST (IAProgram instrs) = do
    checkUniqueMain instrs
    checkUniqueFunctions instrs
    checkUniqueGlobals instrs
validateAST _ = Right ()

checkUniqueMain :: [AST] -> Either String ()
checkUniqueMain instrs =
    let mains = [m | m@(IAMain _ _) <- instrs]
    in if length mains > 1
       then Left "Erreur Sémantique : Le bloc 'principal' est défini plusieurs fois."
       else Right ()

checkUniqueFunctions :: [AST] -> Either String ()
checkUniqueFunctions instrs =
    let names = [name | (IAFunctionDef name _ _ _) <- instrs]
        dups = names \\ nub names
    in case dups of
        (d:_) -> Left $ "Erreur Sémantique : La fonction '" ++ d ++ "' est définie plusieurs fois."
        []    -> Right ()

checkUniqueGlobals :: [AST] -> Either String ()
checkUniqueGlobals instrs =
    let names = [name | (IADeclare name _ _) <- instrs]
        dups = names \\ nub names
    in case dups of
        (d:_) -> Left $ "Erreur Sémantique : La variable globale '" ++ d ++ "' est déclarée plusieurs fois."
        []    -> Right ()