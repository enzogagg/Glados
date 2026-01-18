module JvmBackend.EntryPoint (generateJvm) where

import Types
import JvmBackend.Generator (generateMethods)

generateJvm :: AST -> String -> IO (Either String String)
generateJvm ast _ = do
    let className = "CladProgram"
    let header = ".class public " ++ className ++ "\n"
              ++ ".super java/lang/Object\n\n"
              ++ ".method public <init>()V\n"
              ++ "   aload_0\n"
              ++ "   invokespecial java/lang/Object/<init>()V\n"
              ++ "   return\n"
              ++ ".end method\n\n"

    return $ Right (header ++ generateMethods ast)