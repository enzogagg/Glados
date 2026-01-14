{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ParseToAST
-}

module ParseToAST (
    parseAST
) where

import Types
import CladParser (parseProgramAST)
import Text.Megaparsec (parse, ParseErrorBundle)
import Data.Void (Void)

parseAST :: String -> Either (ParseErrorBundle String Void) AST
parseAST = parse parseProgramAST ""