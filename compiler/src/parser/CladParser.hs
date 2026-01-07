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
import Data.Void()
import Control.Monad (void)

-- ====================================================================
-- Parsing des Types (Annotations)
-- ====================================================================

-- Analyse un type de base (entier, flottant, bool, phrase, neant)
parseBaseType :: Parser CladType
parseBaseType =
      (symbol "entier" >> return IntT)
  <|> (symbol "flottant" >> return FloatT)
  <|> (symbol "pileouface" >> return BoolT)
  <|> (symbol "phrase" >> return StringT)
  <|> (symbol "neant" >> return VoidT)

parseExplicitType :: Parser CladType
parseExplicitType =
      (do
        _ <- symbol "liste"
        _ <- symbol "<"
        elementType <- parseExplicitType
        _ <- symbol ">"
        return (ListT elementType)
      )
  <|> parseBaseType

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
parseTerm :: Parser IAST
parseTerm =
        parseFloat
    <|> parseNumber
    <|> parseBoolean
    <|> parseString
    <|> try parseCall
    <|> (IASymbol <$> parseIdentifier)
    <|> parseListCreation
    <|> (symbol "(" *> parseExpression <* symbol ")")

parseListCreation :: Parser IAST
parseListCreation = do
    _ <- symbol "["
    elements <- parseExpression `sepBy` symbol ","
    _ <- symbol "]"
    return (IAList elements)

parseCall :: Parser IAST
parseCall = do
    name <- parseIdentifier
    _ <- symbol "("
    args <- parseExpression `sepBy` symbol ","
    _ <- symbol ")"
    return (IACall name args)

-- ====================================================================
-- Parsing de la Grammaire (Précédence des Opérateurs)
-- ====================================================================

parseExpression :: Parser IAST
parseExpression = parseRelationnelle

parseRelationnelle :: Parser IAST
parseRelationnelle = chainl1' parseAdditive parseRelationnelleOp
  where
    parseRelationnelleOp =
          (symbol "="  >> return (`IAInfix` "="))
      <|> (symbol "<"  >> return (`IAInfix` "<"))
      <|> (symbol ">"  >> return (`IAInfix` ">"))
      <|> (symbol ">=" >> return (`IAInfix` ">="))
      <|> (symbol "<=" >> return (`IAInfix` "<="))

parseAdditive :: Parser IAST
parseAdditive = chainl1' parseMultiplicative parseAdditiveOp
  where
    parseAdditiveOp =
          (symbol "+" >> return (`IAInfix` "+"))
      <|> (symbol "-" >> return (`IAInfix` "-"))

parseMultiplicative :: Parser IAST
parseMultiplicative = chainl1' parseTerm parseMultiplicativeOp
  where
    parseMultiplicativeOp =
          (symbol "*"   >> return (`IAInfix` "*"))
      <|> (symbol "/"   >> return (`IAInfix` "/"))
      <|> (symbol "mod" >> return (`IAInfix` "mod"))
      <|> (symbol "div" >> return (`IAInfix` "div"))

-- ====================================================================
-- Parsing des Instructions (Blocs, Déclarations, Control Flow)
-- ====================================================================

parseBlockEndKeywords :: Parser ()
parseBlockEndKeywords = void $
        symbol "fin"
    <|> symbol "sinon"
    <|> symbol "sinon si"

parseBlock :: Parser [IAST]
parseBlock = many $ try $ do
    notFollowedBy parseBlockEndKeywords
    parseInstruction

parseInstruction :: Parser IAST
parseInstruction =
        parseFunctionDef -- Doit être en premier pour reconnaître le mot-clé 'fonction'
    <|> parseMain
    <|> parseDeclaration
    <|> parseReturn
    <|> parseConditional
    <|> parseWhile
    <|> parseFor
    <|> try parseAssignment
    <|> parseExpression -- Un appel de fonction seul est une instruction

parseFunctionArgument :: Parser (String, Maybe CladType)
parseFunctionArgument = do
    typeAnnot <- parseExplicitType
    name <- parseIdentifier
    return (name, Just typeAnnot)

-- Définition de fonction complète
parseFunctionDef :: Parser IAST
parseFunctionDef = do
    _ <- symbol "fonction"
    name <- parseIdentifier

    _ <- symbol "("
    params <- parseFunctionArgument `sepBy` symbol ","
    _ <- symbol ")"

    _ <- symbol ":"
    returnTypeAnnot <- parseExplicitType

    body <- parseBlock
    _ <- symbol "fin"

    return (IAFunctionDef name params (Just returnTypeAnnot) body)

parseMain :: Parser IAST
parseMain = do
    _ <- symbol "principal"
    body <- parseBlock
    _ <- symbol "fin"
    return (IAMain body)

parseDeclaration :: Parser IAST
parseDeclaration = do
    _ <- symbol "constante" <|> symbol "variable"

    typeAnnot <- parseExplicitType

    name <- parseIdentifier
    _ <- symbol "="
    IADeclare name (Just typeAnnot) <$> parseExpression

-- Assignation (Modification de variable)
parseAssignment :: Parser IAST
parseAssignment = do
    name <- parseIdentifier
    _ <- symbol "="
    IAAssign name <$> parseExpression

parseReturn :: Parser IAST
parseReturn = do
    _ <- symbol "retourner"
    IAReturn <$> parseExpression

parseElseClause :: Parser (Maybe [IAST])
parseElseClause = optional $ do
    maybeElseIf <- optional parseElseIf
    case maybeElseIf of
        Just elseIf -> return [elseIf]

        Nothing -> do
            _ <- symbol "sinon"
            parseBlock

parseElseIf :: Parser IAST
parseElseIf = do
    _ <- symbol "sinon si"
    _ <- symbol "("
    cond <- parseExpression
    _ <- symbol ")"

    bodyThen <- parseBlock
    IAConditional cond bodyThen <$> parseElseClause

parseConditional :: Parser IAST
parseConditional = do
    _ <- symbol "si"
    _ <- symbol "("
    cond <- parseExpression
    _ <- symbol ")"

    bodyThen <- parseBlock

    elseBody <- parseElseClause

    _ <- symbol "fin"
    return (IAConditional cond bodyThen elseBody)

parseWhile :: Parser IAST
parseWhile = do
    _ <- symbol "tantque"
    _ <- symbol "("
    cond <- parseExpression
    _ <- symbol ")"
    body <- parseBlock
    _ <- symbol "fin"
    return (IAWhile cond body)

parseFor :: Parser IAST
parseFor = do
    _ <- symbol "pour"
    _ <- symbol "("

    initExpr <- parseDeclaration <|> parseAssignment <|> return IAUnit
    _ <- symbol ";"

    condExpr <- parseExpression
    _ <- symbol ";"

    incExpr <- try parseAssignment <|> parseExpression
    _ <- symbol ")"

    body <- parseBlock
    _ <- symbol "fin"

    return (IAFor initExpr condExpr incExpr body)

-- ====================================================================
-- Point d'entrée du Programme CLaD
-- ====================================================================

parseProgramAST :: Parser IAST
parseProgramAST = between sc eof $ do
    instructions <- many parseInstruction
    return (IAProgram instructions)
