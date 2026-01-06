{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CladLexer (Handles tokenization and basic literals)
-}

module CladLexer (
    Parser,
    sc,
    lexeme,
    symbol,
    parseIdentifier,
    parseNumber,
    parseFloat,
    parseBoolean,
    parseString,
    parseOperator
) where

import Types (AST(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

-- Liste des mots-clés réservés en CLaD
reservedWords :: [String]
reservedWords = [
    "fonction", "fin", "principal", "constante", "variable", "retourner",
    "si", "sinon", "sinon si", "tantque", "pour",
    "vrai", "faux", "et", "ou",
    "entier", "flottant", "pileouface", "phrase", "liste", "neant"
    ]

-- ====================================================================
-- Lexer Utility Functions
-- ====================================================================

-- Espace et Commentaires (L.space)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "$$$" "$$$")

-- Lexeme (applique sc après l'analyse d'un token)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbole (un lexeme de chaîne littérale)
symbol :: String -> Parser String
symbol = L.symbol sc

-- Un identifiant valide qui n'est pas un mot-clé réservé
parseIdentifier :: Parser String
parseIdentifier = lexeme $ try $ do
    name <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    if name `elem` reservedWords
        then fail $ "mot-clé réservé : " ++ name
        else return name

-- L'opérateur (utilisé dans les expressions infixées)
parseOperator :: Parser String
parseOperator = lexeme $ many (oneOf "+-*/=<>!%^&|")

-- ====================================================================
-- Lexing des Terminaux (Littéraux) - Retourne des nœuds AST directs
-- ====================================================================

parseNumber :: Parser AST
parseNumber = lexeme $ try $ do
    n <- L.signed sc L.decimal
    return (IANumber n)

parseFloat :: Parser AST
parseFloat = lexeme $ try $ do
    n <- L.signed sc L.float
    return (IAFloatLiteral n)

parseBoolean :: Parser AST
parseBoolean =
        IABoolean True <$ symbol "vrai"
    <|> IABoolean False <$ symbol "faux"

parseString :: Parser AST
parseString = lexeme $ do
    _ <- char '"'
    xs <- many (noneOf ['"'])
    _ <- char '"'
    return (IAString xs)
