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
    <|> (IASymbol <$> parseIdentifier)
    <|> parseCall
    <|> parseListCreation
    <|> (symbol "(" *> parseExpression <* symbol ")")

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
    <|> parseWhile
    <|> parseFor
    <|> parseAssignment
    <|> parseExpression -- Un appel de fonction seul est une instruction

parseFunctionArgument :: Parser (String, Maybe CladType)
parseFunctionArgument = do
    typeAnnot <- parseExplicitType
    name <- parseIdentifier
    return (name, Just typeAnnot)

-- Définition de fonction complète
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
    value <- parseExpression
    return (IAAssign name value)

parseReturn :: Parser AST
parseReturn = do
    _ <- symbol "retourner"
    expr <- parseExpression
    return (IAReturn expr)

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
    cond <- parseExpression
    bodyThen <- parseBlock

    elseBody <- parseElseClause

    return (IAIf cond bodyThen elseBody)

parseConditional :: Parser AST
parseConditional = do
    _ <- symbol "si"
    cond <- parseExpression
    bodyThen <- parseBlock

    elseBody <- parseElseClause

    _ <- symbol "fin"
    return (IAIf cond bodyThen elseBody)

parseWhile :: Parser AST
parseWhile = do
    _ <- symbol "tantque"
    cond <- parseExpression
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
