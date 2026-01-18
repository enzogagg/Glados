module JvmBackend.Generator (generateMethods) where

import Types
import JvmBackend.Context
import JvmBackend.Mapper
import Control.Monad.State

showType :: Maybe CladType -> String
showType (Just IntT)    = "entier"
showType (Just FloatT)  = "flottant"
showType (Just StringT) = "phrase"
showType _              = "entier"

prefix :: String -> String
prefix "entier"   = "i"
prefix "flottant" = "f"
prefix "phrase"   = "a"
prefix _          = "a"

getExprType :: AST -> State JvmContext String
getExprType (IANumber _) = return "entier"
getExprType (IAFloatLiteral _) = return "flottant"
getExprType (IAString _) = return "phrase"
getExprType (IASymbol name) = do
    info <- getVarInfo name
    return $ maybe "entier" snd info
getExprType (IAInfix l _ r) = do
    tL <- getExprType l
    tR <- getExprType r
    if tL == "flottant" || tR == "flottant" then return "flottant" else return "entier"
getExprType _ = return "entier"

generateMethods :: AST -> String
generateMethods (IAProgram instrs) = concatMap generateMethods instrs
generateMethods (IAMain _ body) =
    let (bodyCode, _) = runState (genFunctionBody [] body) emptyContext
    in ".method public static main([Ljava/lang/String;)V\n"
       ++ "  .limit stack 10\n  .limit locals 10\n"
       ++ filterMainReturn bodyCode ++ "  return\n.end method\n\n"
generateMethods (IAFunctionDef name params _ body) =
    let paramNames = map fst params
        (bodyCode, _) = runState (genFunctionBody paramNames body) emptyContext
    in ".method public static " ++ name ++ "(" ++ replicate (length params) 'I' ++ ")I\n"
       ++ "  .limit stack 10\n"
       ++ "  .limit locals " ++ show (length params + 10) ++ "\n"
       ++ bodyCode ++ "  ireturn\n.end method\n\n"
generateMethods _ = ""

filterMainReturn :: String -> String
filterMainReturn code = unlines $ map (\l -> if "ireturn" `elem` words l then "  pop" else l) (lines code)

genFunctionBody :: [String] -> [AST] -> JvmGen
genFunctionBody params body = do
    mapM_ (`reserveVar` "entier") params
    fmap concat (mapM genInstr body)

genInstr :: AST -> JvmGen
genInstr (IANumber n) = return $ "  ldc " ++ show n ++ "\n"
genInstr (IAFloatLiteral f) = return $ "  ldc " ++ show f ++ "\n"
genInstr (IAString s) = return $ "  ldc \"" ++ s ++ "\"\n"

genInstr (IASymbol name) = do
    info <- getVarInfo name
    case info of
        Just (idx, t) -> return $ "  " ++ prefix t ++ "load " ++ show idx ++ "\n"
        Nothing -> return $ "; Error: " ++ name ++ " non definie\n"

genInstr (IADeclare name mType expr) = do
    valCode <- genInstr expr
    let t = showType mType
    idx <- reserveVar name t
    return $ valCode ++ "  " ++ prefix t ++ "store " ++ show idx ++ "\n"

genInstr (IAAssign name expr) = do
    valCode <- genInstr expr
    info <- getVarInfo name
    case info of
        Just (idx, t) -> return $ valCode ++ "  " ++ prefix t ++ "store " ++ show idx ++ "\n"
        Nothing -> return $ "; Error: " ++ name ++ " inconnue\n"

genInstr (IACall "afficher" [expr]) = do
    codeExpr <- genInstr expr
    t <- getExprType expr
    let desc = case t of
                "entier"   -> "(I)V"
                "flottant" -> "(F)V"
                "phrase"   -> "(Ljava/lang/String;)V"
                _          -> "(Ljava/lang/Object;)V"
    return $ "  getstatic java/lang/System/out Ljava/io/PrintStream;\n"
          ++ codeExpr ++ "  invokevirtual java/io/PrintStream/println" ++ desc ++ "\n"

genInstr (IAIf (IAInfix l op r) thenB elseB) = do
    lElse <- uniqueLabel "Else"
    lEnd  <- uniqueLabel "End"
    compCode <- genComparison lElse l op r
    cThen <- fmap concat (mapM genInstr thenB)
    cElse <- maybe (return "") (fmap concat . mapM genInstr) elseB
    return $ compCode ++ cThen ++ "  goto " ++ lEnd ++ "\n" ++ lElse ++ ":\n" ++ cElse ++ lEnd ++ ":\n"

genInstr (IAWhile (IAInfix l op r) body) = do
    lStart <- uniqueLabel "WhileStart"
    lEnd   <- uniqueLabel "WhileEnd"
    compCode <- genComparison lEnd l op r
    bodyCode <- fmap concat (mapM genInstr body)
    return $ lStart ++ ":\n" ++ compCode ++ bodyCode ++ "  goto " ++ lStart ++ "\n" ++ lEnd ++ ":\n"

genInstr (IAInfix l op r)
    | op `elem` ["+", "-", "*", "/"] = do
        cL <- genInstr l
        cR <- genInstr r
        t <- getExprType (IAInfix l op r)
        return $ cL ++ cR ++ "  " ++ mapOp t op ++ "\n"
    | otherwise = return ""

genInstr (IAReturn expr) = do
    code <- genInstr expr
    return $ code ++ "  ireturn\n"

genInstr _ = return ""

genComparison :: String -> AST -> String -> AST -> JvmGen
genComparison label l op r = do
    tL <- getExprType l
    tR <- getExprType r
    codeL <- genInstr l
    codeR <- genInstr r
    let isF = tL == "flottant" || tR == "flottant"
    if isF
        then do
            let loadL = codeL ++ (if tL == "entier" then "  i2f\n" else "")
            let loadR = codeR ++ (if tR == "entier" then "  i2f\n" else "")
            let jmp = case op of ">" -> "ifle"; "<" -> "ifge"; "==" -> "ifne"; "!=" -> "ifeq"; "<=" -> "ifgt"; ">=" -> "iflt"; _ -> "ifne"
            return $ loadL ++ loadR ++ "  fcmpl\n  " ++ jmp ++ " " ++ label ++ "\n"
        else do
            let jmp = case op of ">" -> "if_icmple"; "<" -> "if_icmpge"; "==" -> "if_icmpne"; "!=" -> "if_icmpeq"; "<=" -> "if_icmpgt"; ">=" -> "if_icmplt"; _ -> "if_icmpne"
            return $ codeL ++ codeR ++ "  " ++ jmp ++ " " ++ label ++ "\n"