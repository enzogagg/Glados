module ConstantPropagator (propagateConstants) where

import Types
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- On utilise une Map pour stocker : Nom de variable -> Valeur constante (AST)
type ConstEnv = Map.Map String AST

propagateConstants :: AST -> AST
propagateConstants ast = walk ast Map.empty

walk :: AST -> ConstEnv -> AST
walk (IAProgram instrs) env = IAProgram (walkList instrs env)
walk (IAFunctionDef n p r body) env = IAFunctionDef n p r (walkList body env)
walk (IAMain args body) env = IAMain args (walkList body env)
walk (IABlock instrs) env = IABlock (walkList instrs env)

-- Gestion des déclarations : on enregistre la constante
walk (IADeclare name typ expr) env =
    let expr' = walk expr env
    in case expr' of
        IANumber _ -> IADeclare name typ expr' -- On pourrait propager ici
        IABoolean _ -> IADeclare name typ expr'
        _ -> IADeclare name typ expr'

-- Remplacement des symboles par leur valeur connue
walk (IASymbol name) env = Map.findWithDefault (IASymbol name) name env

-- Sécurité : Si on réassigne, on supprime la variable de la Map
walk (IAAssign name expr) env = IAAssign name (walk expr env)

-- Le reste des nœuds récurse normalement
walk (IAInfix l op r) env = IAInfix (walk l env) op (walk r env)
walk (IAIf c t e) env = IAIf (walk c env) (walkList t env) (fmap (`walkList` env) e)
walk (IAReturn e) env = IAReturn (walk e env)
walk (IACall n args) env = IACall n (map (`walk` env) args)

-- Handle Control Flow with explicit invalidation
walk (IAWhile cond body) env =
    let safeEnv = cleanEnv (cond : body) env
    in IAWhile (walk cond safeEnv) (walkList body safeEnv)

walk (IAFor init cond incr body) env =
    let safeEnv = cleanEnv (init : cond : incr : body) env
    in IAFor (walk init safeEnv) (walk cond safeEnv) (walk incr safeEnv) (walkList body safeEnv)

walk node _ = node

walkList :: [AST] -> ConstEnv -> [AST]
walkList [] _ = []
walkList (instr:rest) env =
    let instr' = walk instr env
        newEnv = updateEnv instr' env
    in instr' : walkList rest newEnv

updateEnv :: AST -> ConstEnv -> ConstEnv
updateEnv (IADeclare name _ val) env = 
    case val of
        IANumber _   -> Map.insert name val env
        IABoolean _  -> Map.insert name val env
        _            -> Map.delete name env
updateEnv (IAAssign name _) env = Map.delete name env
updateEnv (IACall _ _) _ = Map.empty

-- Recursive updates for blocks/loops to ensure environments remain consistent
updateEnv (IAWhile cond body) env = cleanEnv (cond : body) env
updateEnv (IAFor init cond incr body) env = cleanEnv (init : cond : incr : body) env
updateEnv (IABlock stmts) env = cleanEnv stmts env
updateEnv (IAIf cond t e) env = 
    let env' = cleanEnv t env
    in case e of
        Just elseBlock -> cleanEnv elseBlock env'
        Nothing -> env'

updateEnv _ env = env

-- Helper to remove modified variables from Env
cleanEnv :: [AST] -> ConstEnv -> ConstEnv
cleanEnv nodes env =
    if hasCall nodes
    then Map.empty
    else foldr Map.delete env (getModifiedVars nodes)

hasCall :: [AST] -> Bool
hasCall [] = False
hasCall (IACall _ _ : _) = True
hasCall (IAInfix l _ r : rest) = hasCall [l, r] || hasCall rest
hasCall (IAIf c t e : rest) = hasCall (c : t ++ fromMaybe [] e) || hasCall rest
hasCall (IAWhile c b : rest) = hasCall (c : b) || hasCall rest
hasCall (IAFor i c inc b : rest) = hasCall (i : c : inc : b) || hasCall rest
hasCall (IABlock b : rest) = hasCall b || hasCall rest
hasCall (IADeclare _ _ e : rest) = hasCall [e] || hasCall rest
hasCall (IAAssign _ e : rest) = hasCall [e] || hasCall rest
hasCall (IAReturn e : rest) = hasCall [e] || hasCall rest
hasCall (IAList l : rest) = hasCall l || hasCall rest
hasCall (IATuple l : rest) = hasCall l || hasCall rest
hasCall (_ : rest) = hasCall rest

getModifiedVars :: [AST] -> [String]
getModifiedVars [] = []
getModifiedVars (IAAssign name _ : rest) = name : getModifiedVars rest
getModifiedVars (IAIf _ t e : rest) = getModifiedVars t ++ matches e ++ getModifiedVars rest
    where matches Nothing = []
          matches (Just s) = getModifiedVars s
getModifiedVars (IAWhile _ b : rest) = getModifiedVars b ++ getModifiedVars rest
getModifiedVars (IAFor _ _ _ b : rest) = getModifiedVars b ++ getModifiedVars rest
getModifiedVars (IABlock b : rest) = getModifiedVars b ++ getModifiedVars rest
getModifiedVars (_ : rest) = getModifiedVars rest