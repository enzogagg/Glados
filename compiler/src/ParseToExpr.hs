{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ParseToExpr
-}

module ParseToExpr (
    parseExpr,
    parseProgram
) where

import Control.Monad ()
import Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseProgram :: Parser [Expr]
parseProgram = between sc eof (many parseExpr)

parseExpr :: Parser Expr
parseExpr =
        parseBoolean
    <|> parseList
    <|> parseFloat
    <|> parseNumber
    <|> parseString
    <|> parseSymbol

parseNumber :: Parser Expr
parseNumber = lexeme $ try $ do
    sign <- optional (oneOf "+-")
    firstDigit <- digitChar
    rest <- many digitChar
    let n = read (maybe "" (:[]) sign ++ (firstDigit : rest)) :: Integer
    return (Number n)

parseFloat :: Parser Expr
parseFloat = lexeme $ try $ do
    sign <- optional (oneOf "+-")
    firstDigit <- digitChar
    _ <- char '.'
    rest <- many digitChar
    let n = read (maybe "" (:[]) sign ++ (firstDigit : '.' : rest)) :: Double
    return (FloatLiteral n)

parseBoolean :: Parser Expr
parseBoolean =
        Boolean True <$ symbol "#t"
    <|> Boolean False <$ symbol "#f"

parseSymbol :: Parser Expr
parseSymbol = lexeme $ do
    first <- letterChar <|> oneOf "+-*/=<>?!"
    rest  <- many (alphaNumChar <|> oneOf "+-*/=<>?!")
    return (Symbol (first : rest))

parseString :: Parser Expr
parseString = lexeme $ do
    _ <- char '"'
    xs <- many (noneOf ['"'])
    _ <- char '"'
    return (String xs)

parseList :: Parser Expr
parseList = do
    _ <- symbol "("
    xs <- many parseExpr
    _ <- symbol ")"
    return (List xs)
