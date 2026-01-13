module ConstantPropagator (propagateConstants) where

import Types
import qualified Data.Map as Map

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
updateEnv _ env = env