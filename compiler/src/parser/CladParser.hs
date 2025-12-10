{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CladParser (Handles AST construction and precedence)
-}

module CladParser (
    parseProgramAST,
    parseInstruction,
    parseExpression
) where

import Types
import CladLexer
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

-- ====================================================================
-- Parsing des Expressions (Terminaux simples et appels de fonction)
-- ====================================================================

-- overloaded chainl1 car erreur quand import de Megaparsec
chainl1' :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1' p op = do
    a <- p
    rest a
  where
    rest a =
        (do
            f <- op
            b <- p
            rest (f a b)
        )
        <|> return a

-- Analyseur de base pour les éléments qui ne nécessitent pas de priorité (littéraux, identifiants, appels)
parseTerm :: Parser AST
parseTerm =
        parseFloat
    <|> parseNumber
    <|> parseBoolean
    <|> parseString
    <|> (IASymbol <$> parseIdentifier) -- Identifiant
    <|> parseCall
    <|> parseListCreation
    <|> (symbol "(" *> parseExpression <* symbol ")") -- Expression entre parenthèses

parseListCreation :: Parser AST
parseListCreation = do
    _ <- symbol "["
    elements <- parseExpression `sepBy` symbol ","
    _ <- symbol "]"
    return (IAList elements)

parseCall :: Parser AST
parseCall = do
    name <- parseIdentifier
    _ <- symbol "("
    args <- parseExpression `sepBy` symbol ","
    _ <- symbol ")"
    return (IACall name args)

-- ====================================================================
-- Parsing de la Grammaire (Précédence des Opérateurs)
-- ====================================================================

parseExpression :: Parser AST
parseExpression = parseRelationnelle

parseRelationnelle :: Parser AST
parseRelationnelle = chainl1' parseAdditive parseRelationnelleOp
  where
    parseRelationnelleOp =
          (symbol "="  >> return (\left right -> IAInfix left "=" right))
      <|> (symbol "<"  >> return (\left right -> IAInfix left "<" right))
      <|> (symbol ">"  >> return (\left right -> IAInfix left ">" right))
      <|> (symbol ">=" >> return (\left right -> IAInfix left ">=" right))
      <|> (symbol "<=" >> return (\left right -> IAInfix left "<=" right))

parseAdditive :: Parser AST
parseAdditive = chainl1' parseMultiplicative parseAdditiveOp
  where
    parseAdditiveOp =
          (symbol "+" >> return (\left right -> IAInfix left "+" right))
      <|> (symbol "-" >> return (\left right -> IAInfix left "-" right))

parseMultiplicative :: Parser AST
parseMultiplicative = chainl1' parseTerm parseMultiplicativeOp
  where
    parseMultiplicativeOp =
          (symbol "*"   >> return (\left right -> IAInfix left "*" right))
      <|> (symbol "/"   >> return (\left right -> IAInfix left "/" right))
      <|> (symbol "mod" >> return (\left right -> IAInfix left "mod" right))
      <|> (symbol "div" >> return (\left right -> IAInfix left "div" right))

-- ====================================================================
-- Parsing des Instructions (Blocs, Déclarations, Control Flow)
-- ====================================================================

-- Un bloc est une série d'instructions jusqu'à 'fin' ou EOF
parseBlock :: Parser [AST]
parseBlock = many parseInstruction

parseInstruction :: Parser AST
parseInstruction =
        parseFunctionDef -- Doit être en premier pour reconnaître le mot-clé 'fonction'
    <|> parseMain
    <|> parseDeclaration
    <|> parseReturn
    <|> parseConditional
    -- TODO: parseWhile
    -- TODO: parseFor
    <|> parseAssignment
    <|> parseExpression -- Un appel de fonction seul est une instruction

-- TODO: parseFunctionDef (nécessite de gérer les annotations de type dans Types.hs)
parseFunctionDef :: Parser AST
parseFunctionDef = do
    _ <- symbol "fonction"
    name <- parseIdentifier
    _ <- symbol "("
    -- Pour l'instant on gère juste le nom du paramètre (String, Nothing)
    params <- ((\n -> (n, Nothing)) <$> parseIdentifier) `sepBy` symbol ","
    _ <- symbol ")"
    -- Pas de gestion du type de retour pour l'instant (Nothing)
    body <- parseBlock
    _ <- symbol "fin"
    return (IAFunctionDef name params Nothing body)

parseMain :: Parser AST
parseMain = do
    _ <- symbol "principal"
    body <- parseBlock
    _ <- symbol "fin"
    return (IAMain body)

-- Déclaration de constante ou variable
parseDeclaration :: Parser AST
parseDeclaration = do
    -- Commence par le mot-clé
    keyword <- symbol "constante" <|> symbol "variable"
    name <- parseIdentifier
    _ <- symbol "=" -- OBLIGATOIRE : L'opérateur d'assignation explicite
    value <- parseExpression
    return (IADeclare name Nothing value)

-- Assignation (Modification de variable)
parseAssignment :: Parser AST
parseAssignment = do
    name <- parseIdentifier
    _ <- symbol "="
    value <- parseExpression
    return (IAAssign name value)

parseReturn :: Parser AST
parseReturn = do
    _ <- symbol "retourner"
    expr <- parseExpression
    return (IAReturn expr)

-- Fonction simple (simplification du 'si...sinon si...sinon')
parseConditional :: Parser AST
parseConditional = do
    _ <- symbol "si"
    cond <- parseExpression
    bodyThen <- parseBlock
    -- Simplification: gère juste 'si ... fin' ou 'si ... sinon ... fin'
    elseBody <- optional $ do
        _ <- symbol "sinon"
        parseBlock
    _ <- symbol "fin"
    return (IAIf cond bodyThen elseBody)

-- ====================================================================
-- Point d'entrée du Programme CLaD
-- ====================================================================

parseProgramAST :: Parser AST
parseProgramAST = between sc eof $ do
    instructions <- many parseInstruction
    return (IAProgram instructions)