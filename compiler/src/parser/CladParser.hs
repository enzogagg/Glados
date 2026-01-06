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
import Control.Monad()

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
parseTerm :: Parser AST
parseTerm =
        parseFloat
    <|> parseNumber
    <|> parseBoolean
    <|> parseString
    <|> parseUnaryNot
    <|> try parseCall
    <|> (IASymbol <$> parseIdentifier)
    <|> parseListCreation
    <|> (symbol "(" *> parseExpression <* symbol ")")

parseUnaryNot :: Parser AST
parseUnaryNot = do
    _ <- symbol "!"
    term <- parseTerm
    return (IACall "!" [term])

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
parseExpression = parseLogicalOr

parseLogicalOr :: Parser AST
parseLogicalOr = chainl1' parseLogicalAnd parseOrOp
  where
    parseOrOp = (symbol "ou" >> return (`IAInfix` "ou"))

parseLogicalAnd :: Parser AST
parseLogicalAnd = chainl1' parseRelationnelle parseAndOp
  where
    parseAndOp = (symbol "et" >> return (`IAInfix` "et"))

parseRelationnelle :: Parser AST
parseRelationnelle = chainl1' parseAdditive parseRelationnelleOp
  where
    parseRelationnelleOp =
          (symbol "=="  >> return (`IAInfix` "=="))
      <|> (symbol "<"  >> return (`IAInfix` "<"))
      <|> (symbol ">"  >> return (`IAInfix` ">"))
      <|> (symbol ">=" >> return (`IAInfix` ">="))
      <|> (symbol "<=" >> return (`IAInfix` "<="))

parseAdditive :: Parser AST
parseAdditive = chainl1' parseMultiplicative parseAdditiveOp
  where
    parseAdditiveOp =
          (symbol "+" >> return (`IAInfix` "+"))
      <|> (symbol "-" >> return (`IAInfix` "-"))

parseMultiplicative :: Parser AST
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

parseBlock :: Parser [AST]
parseBlock = many parseInstruction

parseInstruction :: Parser AST
parseInstruction =
        parseFunctionDef
    <|> parseMain
    <|> parseDeclaration
    <|> parseReturn
    <|> parseConditional
    <|> parseWhile
    <|> parseFor
    <|> try parseAssignment
    <|> parseExpression

parseFunctionArgument :: Parser (String, Maybe CladType)
parseFunctionArgument = do
    typeAnnot <- parseExplicitType
    name <- parseIdentifier
    return (name, Just typeAnnot)

parseFunctionDef :: Parser AST
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

parseMain :: Parser AST
parseMain = do
    _ <- symbol "principal"
    body <- parseBlock
    _ <- symbol "fin"
    return (IAMain body)

parseDeclaration :: Parser AST
parseDeclaration = do
    _ <- symbol "constante" <|> symbol "variable"

    typeAnnot <- parseExplicitType

    name <- parseIdentifier
    _ <- symbol "="
    value <- parseExpression

    return (IADeclare name (Just typeAnnot) value)

-- Assignation (Modification de variable)
parseAssignment :: Parser AST
parseAssignment = do
    name <- parseIdentifier
    _ <- symbol "="
    IAAssign name <$> parseExpression

parseReturn :: Parser AST
parseReturn = do
    _ <- symbol "retourner"
    IAReturn <$> parseExpression

parseElseClause :: Parser (Maybe [AST])
parseElseClause = optional $ do
    maybeElseIf <- optional parseElseIf
    case maybeElseIf of
        Just elseIf -> return [elseIf]

        Nothing -> do
            _ <- symbol "sinon"
            parseBlock

parseElseIf :: Parser AST
parseElseIf = do
    _ <- symbol "sinon si"
    _ <- symbol "("
    cond <- parseExpression
    _ <- symbol ")"

    bodyThen <- parseBlock

    elseBody <- parseElseClause

    return (IAIf cond bodyThen elseBody)

parseConditional :: Parser AST
parseConditional = do
    _ <- symbol "si"
    _ <- symbol "("
    cond <- parseExpression
    _ <- symbol ")"

    bodyThen <- parseBlock

    elseBody <- parseElseClause

    _ <- symbol "fin"
    return (IAIf cond bodyThen elseBody)

parseWhile :: Parser AST
parseWhile = do
    _ <- symbol "tantque"
    _ <- symbol "("
    cond <- parseExpression
    _ <- symbol ")"
    body <- parseBlock
    _ <- symbol "fin"
    return (IAWhile cond body)

parseFor :: Parser AST
parseFor = do
    _ <- symbol "pour"
    _ <- symbol "("

    initExpr <- parseDeclaration <|> parseAssignment <|> return IAUnit
    _ <- symbol ";"

    condExpr <- parseExpression
    _ <- symbol ";"

    incExpr <- parseExpression
    _ <- symbol ")"

    body <- parseBlock
    _ <- symbol "fin"

    return (IAFor initExpr condExpr incExpr body)

-- ====================================================================
-- Point d'entrée du Programme CLaD
-- ====================================================================

parseProgramAST :: Parser AST
parseProgramAST = between sc eof $ do
    instructions <- many parseInstruction
    return (IAProgram instructions)
