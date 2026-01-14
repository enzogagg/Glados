module AstToClass (parseClass) where

import Types
import qualified JvmBackend.EntryPoint as Jvm
import System.IO()

parseClass :: AST -> String -> IO (Either String ())
parseClass ast outputName = do
    result <- Jvm.generateJvm ast outputName
    case result of
        Left err -> return $ Left err
        Right code -> do
            writeFile (outputName ++ ".j") code
            putStrLn $ "Jasmin source generated: " ++ outputName ++ ".j\n"
                ++ "Please compile it with Jasmin to get the .class file.\n"
                ++ "java -jar jasmin.jar " ++ outputName ++ ".j"
            return $ Right ()