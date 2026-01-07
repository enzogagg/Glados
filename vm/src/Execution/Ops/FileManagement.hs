{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- File management operations
-}

module Execution.Ops.FileManagement where

import Types (Value(..))
import Execution.State (VMState(..))

import System.IO
import System.Directory (doesFileExist)
import Control.Exception (try, IOException)

opOpenFile :: VMState -> IO (Either String VMState)
opOpenFile state =
    case stack state of
        (StringVal mode : StringVal path : rest) -> do
            case mode of
                "r" -> do
                    exists <- doesFileExist path
                    if exists
                        then return $ Right $ state { stack = FileVal path : rest }
                        else return $ Left $ "Error: File not found: " ++ path
                "w" -> return $ Right $ state { stack = FileVal path : rest }
                "a" -> return $ Right $ state { stack = FileVal path : rest }
                _   -> return $ Left $ "Error: Invalid file mode '" ++ mode ++ "'"
        _ -> return $ Left "Error: OpenFile requires [String path, String mode] on the stack"

opReadFile :: VMState -> IO (Either String VMState)
opReadFile state =
    case stack state of
        (FileVal path : rest) -> do
            result <- try (readFile path) :: IO (Either IOException String)
            case result of
                Right content -> return $ Right $ state { stack = StringVal content : rest }
                Left ex -> return $ Left $ "Error: Read failed for '" ++ path ++ "': " ++ show ex
        _ -> return $ Left "Error: ReadFile requires a FileVal on the stack"

opWriteFile :: VMState -> IO (Either String VMState)
opWriteFile state =
    case stack state of
        (StringVal content : FileVal path : rest) -> do
            result <- try (writeFile path content) :: IO (Either IOException ())
            case result of
                Right () -> return $ Right $ state { stack = rest }
                Left ex -> return $ Left $ "Error: Write failed for '" ++ path ++ "': " ++ show ex
        _ -> return $ Left "Error: WriteFile requires [String content, FileVal path] on the stack"

opCloseFile :: VMState -> IO (Either String VMState)
opCloseFile state =
    case stack state of
        (FileVal _ : rest) -> return $ Right $ state { stack = rest }
        _ -> return $ Left "Error: CloseFile requires a FileVal on the stack"
