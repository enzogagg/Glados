{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Interpreter
-}

module Interpreter (runREPL, evalExpr) where

import Types
import CladParser (parseExpression, parseInstruction)
import Text.Megaparsec (parse, errorBundlePretty)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map
import Control.Exception (try, SomeException)
import System.Directory (doesFileExist)

-- ====================================================================
-- Boucle REPL (Sécurisée)
-- ====================================================================

runREPL :: IO ()
runREPL = replLoop Map.empty

replLoop :: Env -> IO ()
replLoop env = do
    putStr "clad> "
    hFlush stdout
    input <- readMultiLine "" 0 
    if input == "quitter" 
        then putStrLn "Au revoir !"
        else do
            result <- try $ handleInput env input
            case result of
                Left err -> do
                    putStrLn $ "Erreur : " ++ show (err :: SomeException)
                    replLoop env
                Right newEnv -> replLoop newEnv

readMultiLine :: String -> Int -> IO String
readMultiLine acc balance = do
    line <- getLine
    let newAcc = if null acc then line else acc ++ "\n" ++ line
    let newBalance = balance + countOpenings line - countClosings line
    
    if newBalance <= 0 && not (null line)
        then return newAcc
        else do
            putStr "      " 
            hFlush stdout
            readMultiLine newAcc newBalance

countOpenings :: String -> Int
countOpenings s = length $ filter (`elem` ["fonction", "si", "tantque", "pour"]) (words s)

countClosings :: String -> Int
countClosings s = length $ filter (== "fin") (words s)

handleInput :: Env -> String -> IO Env
handleInput env input = do
    let resInst = parse parseInstruction "" input
    case resInst of
        Right ast -> do
            (val, nextEnv) <- evalInstruction env ast
            case ast of
                IADeclare {} -> return nextEnv
                IAAssign {}  -> return nextEnv
                _            -> print val >> return nextEnv
        Left _ -> case parse parseExpression "" input of
            Right ast -> do
                val <- evalExpr env ast
                print val
                return env
            Left err -> do
                putStrLn (errorBundlePretty err)
                return env

-- ====================================================================
-- Évaluateur (Utilise error pour les messages, 'try' les capturera)
-- ====================================================================

checkType :: CladType -> Value -> Bool
checkType IntT (IntVal _)       = True
checkType FloatT (FloatVal _)   = True
checkType StringT (StringVal _) = True
checkType BoolT (BoolVal _)     = True
checkType (TupleT ts) (TupleVal vs) = 
    length ts == length vs && all (uncurry checkType) (zip ts vs)
checkType (ListT t) (ListVal vs) = 
    all (checkType t) vs
checkType _ _ = False

evalInstruction :: Env -> AST -> IO (Value, Env)
evalInstruction env (IADeclare name (Just expectedType) expr) = do
    val <- evalExpr env expr
    if checkType expectedType val
        then return (val, Map.insert name val env)
        else error $ "Type incorrect pour '" ++ name ++ "'. Attendu: " ++ show expectedType
evalInstruction env (IAAssign name expr) = do
    val <- evalExpr env expr
    if Map.member name env
        then return (val, Map.insert name val env)
        else error $ "Variable '" ++ name ++ "' non définie."
evalInstruction env (IAFunctionDef name args _ body) = do
    let argNames = map fst args
    let func = FuncVal argNames body env
    return (Void, Map.insert name func env)
evalInstruction env ast = do
    val <- evalExpr env ast
    return (val, env)

evalExpr :: Env -> AST -> IO Value
evalExpr _   (IANumber n)   = return $ IntVal n
evalExpr _   (IABoolean b)  = return $ BoolVal b
evalExpr _   (IAString s)   = return $ StringVal s
evalExpr env (IASymbol s)   = case Map.lookup s env of
    Just val -> return val
    Nothing  -> error $ "Variable inconnue: " ++ s

evalExpr env (IAIf condition thenBody maybeElseBody) = do
    condVal <- evalExpr env condition
    case condVal of
        BoolVal True -> evalBody env thenBody
        BoolVal False -> case maybeElseBody of
            Just elseBody -> evalBody env elseBody
            Nothing       -> return Void
        _ -> error "La condition du 'si' doit être un pileouface (booléen)."

evalExpr env (IACall name argsExprs) = do
    argVals <- mapM (evalExpr env) argsExprs
    case Map.lookup name env of
        Just (FuncVal argNames body closureEnv) -> do
            if length argNames /= length argVals
                then error $ "La fonction " ++ name ++ " attend " ++ show (length argNames) ++ " arguments."
                else do
                    let localEnv = Map.fromList (zip argNames argVals) 
                                   `Map.union` Map.insert name (FuncVal argNames body closureEnv) closureEnv
                    evalBody localEnv body
        _ -> handleBuiltin name argVals

evalExpr env (IATuple exprs) = TupleVal <$> mapM (evalExpr env) exprs
evalExpr env (IAList exprs)  = ListVal <$> mapM (evalExpr env) exprs
evalExpr env (IAInfix left op right) = do
    v1 <- evalExpr env left
    v2 <- evalExpr env right
    return $ applyBinaryOp op v1 v2
evalExpr env (IAReturn expr) = do
    evalExpr env expr

evalExpr _ _ = error "Instruction non supportée ou non implémentée."

evalBody :: Env -> [AST] -> IO Value
evalBody _ [] = return Void
evalBody env (inst:rest) = do
    case inst of
        IAReturn expr -> evalExpr env expr
        _ -> do
            (val, newEnv) <- evalInstruction env inst
            if null rest 
                then return val
                else evalBody newEnv rest

-- La logique des built-ins sécurisée
handleBuiltin :: String -> [Value] -> IO Value
handleBuiltin "tete" [ListVal l] = case l of
    (x:_) -> return x
    []    -> error "Erreur : 'tete' sur une liste vide"

handleBuiltin "reste" [ListVal l] = case l of
    (_:xs) -> return (ListVal xs)
    []     -> error "Erreur : 'reste' sur une liste vide"

handleBuiltin "est_vide" [ListVal l] = return (BoolVal (null l))

handleBuiltin "fusion" [el, ListVal l] = return (ListVal (el:l))

handleBuiltin "ajouter" [ListVal l, el] = return (ListVal (l ++ [el]))

handleBuiltin "taille" [ListVal l] = return (IntVal (fromIntegral $ length l))
handleBuiltin "taille" [TupleVal t] = return (IntVal (fromIntegral $ length t))

handleBuiltin "nieme" [ListVal l, IntVal i] = 
    let idx = fromIntegral i 
    in if idx < 0 || idx >= length l 
       then error $ "Erreur : Index " ++ show idx ++ " hors limites pour la liste"
       else return (l !! idx)

handleBuiltin "nieme" [TupleVal t, IntVal i] = 
    let idx = fromIntegral i 
    in if idx < 0 || idx >= length t
       then error $ "Erreur : Index " ++ show idx ++ " hors limites pour le tuple"
       else return (t !! idx)

handleBuiltin "contient" [ListVal l, el] = return (BoolVal (el `elem` l))

handleBuiltin "vider" [_] = return (ListVal [])

handleBuiltin "supprimer" [ListVal l, IntVal i] = 
    let idx = fromIntegral i
    in if idx < 0 || idx >= length l
       then error "Erreur : Index hors limites pour 'supprimer'"
       else case splitAt idx l of
              (before, _:after) -> return (ListVal (before ++ after))
              (before, [])      -> return (ListVal before)

handleBuiltin "inserer" [ListVal l, IntVal i, el] = 
    let idx = fromIntegral i
    in if idx < 0 || idx > length l
       then error "Erreur : Index hors limites pour 'inserer'"
       else let (before, after) = splitAt idx l
            in return (ListVal (before ++ [el] ++ after))

handleBuiltin "lire" [StringVal path] = do
    exists <- doesFileExist path
    if exists 
        then StringVal <$> readFile path
        else error $ "Erreur : Le fichier '" ++ path ++ "' n'existe pas"

handleBuiltin "ecrire" [StringVal path, StringVal txt] = 
    writeFile path txt >> return Void

handleBuiltin "ouvert" [StringVal path] = return (StringVal path)
handleBuiltin "fermer" [_] = return Void

handleBuiltin "afficher" [v] = do
    putStrLn (cleanShow v)
    return Void

handleBuiltin name args = error $ "Erreur : Built-in '" ++ name ++ "' appelé avec des arguments invalides : " ++ show args

cleanShow :: Value -> String
cleanShow (StringVal s) = s
cleanShow v = show v

-- ====================================================================
-- Opérations Binaires
-- ====================================================================

applyBinaryOp :: String -> Value -> Value -> Value
applyBinaryOp "+" (IntVal a) (IntVal b) = IntVal (a + b)
applyBinaryOp "-" (IntVal a) (IntVal b) = IntVal (a - b)
applyBinaryOp "*" (IntVal a) (IntVal b) = IntVal (a * b)
applyBinaryOp "div" (IntVal a) (IntVal b) 
    | b == 0    = error "Division par zéro !"
    | otherwise = IntVal (a `div` b)
applyBinaryOp "/" (IntVal a) (IntVal b) 
    | b == 0    = error "Division par zéro !"
    | otherwise = IntVal (a `div` b)
applyBinaryOp "mod" (IntVal a) (IntVal b)
    | b == 0    = error "Modulo par zéro !"
    | otherwise = IntVal (a `mod` b)

applyBinaryOp "==" v1 v2 = BoolVal (v1 == v2)
applyBinaryOp "!=" v1 v2 = BoolVal (v1 /= v2)
applyBinaryOp ">=" (IntVal a) (IntVal b) = BoolVal (a >= b)
applyBinaryOp "<=" (IntVal a) (IntVal b) = BoolVal (a <= b)
applyBinaryOp ">"  (IntVal a) (IntVal b) = BoolVal (a > b)
applyBinaryOp "<"  (IntVal a) (IntVal b) = BoolVal (a < b)
applyBinaryOp "et" (BoolVal a) (BoolVal b) = BoolVal (a && b)
applyBinaryOp "ou" (BoolVal a) (BoolVal b) = BoolVal (a || b)
applyBinaryOp op v1 v2 = error $ "Opération impossible avec '" ++ op ++ "' entre " ++ show v1 ++ " et " ++ show v2