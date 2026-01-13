module JvmBackend.Generator (generateMethods) where

import Types
import JvmBackend.Context
import JvmBackend.Mapper
import Control.Monad.State

-- | Point d'entrée pour la génération de toutes les méthodes d'un programme
generateMethods :: AST -> String
generateMethods (IAProgram instrs) = concatMap generateMethods instrs

-- | Gestion du point d'entrée principal (Main)
generateMethods (IAMain _ body) =
    let (bodyCode, _) = runState (genFunctionBody [] body) emptyContext
        cleanBody = filterMainReturn bodyCode
    in ".method public static main([Ljava/lang/String;)V\n"
       ++ "  .limit stack 10\n"
       ++ "  .limit locals 10\n"
       ++ cleanBody
       ++ "  return\n"
       ++ ".end method\n\n"

-- | Gestion des définitions de fonctions classiques
generateMethods (IAFunctionDef name params _ body) =
    let paramNames = map fst params
        (bodyCode, _) = runState (genFunctionBody paramNames body) emptyContext
    in ".method public static " ++ name ++ "(" ++ replicate (length params) 'I' ++ ")I\n"
       ++ "  .limit stack 10\n"
       ++ "  .limit locals " ++ show (length params + 10) ++ "\n"
       ++ bodyCode
       ++ "  ireturn\n"
       ++ ".end method\n\n"

generateMethods _ = ""

-- | Nettoyage pour éviter le VerifyError dans le main (pas de ireturn autorisé)
filterMainReturn :: String -> String
filterMainReturn code =
    let rows = lines code
        cleanRows = map (\l -> if "ireturn" `elem` words l then "  pop" else l) rows
    in unlines cleanRows

-- | Génération du corps d'une fonction (réservation des variables locales)
genFunctionBody :: [String] -> [AST] -> JvmGen
genFunctionBody params body = do
    mapM_ reserveVar params
    codes <- mapM genInstr body
    return $ concat codes

-- | Générateur d'instructions principal
genInstr :: AST -> JvmGen
genInstr (IANumber n) = return $ "  ldc " ++ show n ++ "\n"

genInstr (IASymbol name) = do
    maybeIdx <- getVarIndex name
    case maybeIdx of
        Just idx -> return $ "  iload " ++ show idx ++ "\n"
        Nothing  -> return $ "; Error: Variable " ++ name ++ " non definie\n"

genInstr (IADeclare name _ expr) = do
    valCode <- genInstr expr
    idx <- reserveVar name
    return $ valCode ++ "  istore " ++ show idx ++ "\n"

genInstr (IAAssign name expr) = do
    valCode <- genInstr expr
    maybeIdx <- getVarIndex name
    case maybeIdx of
        Just idx -> return $ valCode ++ "  istore " ++ show idx ++ "\n"
        Nothing  -> return $ "; Error: Variable " ++ name ++ " inconnue\n"

-- Affichage (Print)
genInstr (IACall "afficher" [expr]) = do
    codeExpr <- genInstr expr
    return $ "  getstatic java/lang/System/out Ljava/io/PrintStream;\n"
          ++ codeExpr
          ++ "  invokevirtual java/io/PrintStream/println(I)V\n"

-- Appel de fonction
genInstr (IACall name args) = do
    codeArgs <- mapM genInstr args
    return $ concat codeArgs
          ++ "  invokestatic CladProgram/" ++ name ++ "(" ++ replicate (length args) 'I' ++ ")I\n"

genInstr (IAReturn expr) = do
    codeExpr <- genInstr expr
    return $ codeExpr ++ "  ireturn\n"

-- Structure SI (IAIf) avec optimisation des branchements
genInstr (IAIf (IAInfix l op r) thenBranch maybeElseBranch) = do
    labelElse <- uniqueLabel "Else"
    labelEnd <- uniqueLabel "End"
    codeL <- genInstr l
    codeR <- genInstr r
    let jumpOp = case op of
                   "==" -> "if_icmpne"
                   "!=" -> "if_icmpeq"
                   "<"  -> "if_icmpge"
                   ">"  -> "if_icmple"
                   "<=" -> "if_icmpgt"
                   ">=" -> "if_icmplt"
                   _    -> "ifeq"
    codeThen <- fmap concat (mapM genInstr thenBranch)
    codeElse <- case maybeElseBranch of
        Just eb -> fmap concat (mapM genInstr eb)
        Nothing -> return ""
    return $ codeL ++ codeR
          ++ "  " ++ jumpOp ++ " " ++ labelElse ++ "\n"
          ++ codeThen
          ++ "  goto " ++ labelEnd ++ "\n"
          ++ labelElse ++ ":\n"
          ++ codeElse
          ++ labelEnd ++ ":\n"

-- Structure TANTQUE (IAWhile)
genInstr (IAWhile cond body) = do
    labelStart <- uniqueLabel "WhileStart"
    labelEnd <- uniqueLabel "WhileEnd"
    codeCond <- genInstr cond
    codeBody <- fmap concat (mapM genInstr body)
    return $ labelStart ++ ":\n"
          ++ codeCond
          ++ "  ifeq " ++ labelEnd ++ "\n"
          ++ codeBody
          ++ "  goto " ++ labelStart ++ "\n"
          ++ labelEnd ++ ":\n"

-- Structure POUR (IAFor)
genInstr (IAFor init cond step body) = do
    labelStart <- uniqueLabel "ForStart"
    labelEnd <- uniqueLabel "ForEnd"
    codeInit <- genInstr init
    codeCond <- genInstr cond
    codeStep <- genInstr step
    codeBody <- fmap concat (mapM genInstr body)
    return $ codeInit
          ++ labelStart ++ ":\n"
          ++ codeCond
          ++ "  ifeq " ++ labelEnd ++ "\n"
          ++ codeBody
          ++ codeStep
          ++ "  goto " ++ labelStart ++ "\n"
          ++ labelEnd ++ ":\n"

-- Opérations Infixes (Calculs ou Comparaisons autonomes)
genInstr (IAInfix l op r)
    -- Calculs arithmétiques
    | op `elem` ["+", "-", "*", "/"] = do
        codeL <- genInstr l
        codeR <- genInstr r
        return $ codeL ++ codeR ++ "  " ++ mapOp op ++ "\n"
    -- Comparaisons laissant 0 ou 1 sur la pile (nécessaire pour For/While)
    | op `elem` ["==", "!=", "<", ">", "<=", ">="] = do
        labelTrue <- uniqueLabel "cmpTrue"
        labelEnd  <- uniqueLabel "cmpEnd"
        codeL <- genInstr l
        codeR <- genInstr r
        let jumpOp = case op of
                       "==" -> "if_icmpeq"
                       "!=" -> "if_icmpne"
                       "<"  -> "if_icmplt"
                       ">"  -> "if_icmpgt"
                       "<=" -> "if_icmple"
                       ">=" -> "if_icmpge"
                       _    -> "ifne"
        return $ codeL ++ codeR
              ++ "  " ++ jumpOp ++ " " ++ labelTrue ++ "\n"
              ++ "  ldc 0\n"
              ++ "  goto " ++ labelEnd ++ "\n"
              ++ labelTrue ++ ":\n"
              ++ "  ldc 1\n"
              ++ labelEnd ++ ":\n"
    | otherwise = return $ "; Erreur: Operateur " ++ op ++ " non supporte\n"

genInstr _ = return ""