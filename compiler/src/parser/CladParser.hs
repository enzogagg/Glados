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
import Text.Megaparsec.Char (char)
import Data.Void()
import Control.Monad (void)

-- ====================================================================
-- Parsing des Types (Annotations)
-- ====================================================================

-- Analyse un type de base (entier, flottant, bool, phrase, neant)
parseBaseType :: Parser CladType
parseBaseType =
      (keyword "entier" >> return IntT)
  <|> (keyword "flottant" >> return FloatT)
  <|> (keyword "pileouface" >> return BoolT)
  <|> (keyword "phrase" >> return StringT)
  <|> (keyword "caractere" >> return CharT)
  <|> (keyword "neant" >> return VoidT)
  <|> (keyword "fichier" >> return FileT)
  <|> (keyword "tableau" >> return (ArrayT AnyT))
  <|> (keyword "dictionnaire" >> return (MapT AnyT AnyT))
  <|> (keyword "structure" >> return StructT)

parseTupleType :: Parser CladType
parseTupleType = do
    _ <- symbol "("
    types <- parseExplicitType `sepBy1` symbol ","
    _ <- symbol ")"
    return (TupleT types)

parseExplicitType :: Parser CladType
parseExplicitType =
      try parseTupleType
  <|> (do
        _ <- keyword "liste"
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
        parseUnit
    <|> parseFloat
    <|> parseNumber
    <|> parseBoolean
    <|> parseChar
    <|> parseString
    <|> try parseUnaryNot
    <|> try parseCall
    <|> (IASymbol <$> parseIdentifier)
    <|> parseListCreation
    <|> parseTupleOrParenthesized

parseUnit :: Parser AST
parseUnit = keyword "neant" >> return IAUnit

parseTupleOrParenthesized :: Parser AST
parseTupleOrParenthesized = do
    _ <- symbol "("
    first <- parseExpression
    maybeRest <- optional (symbol "," *> parseExpression `sepBy` symbol ",")
    _ <- symbol ")"
    case maybeRest of
        Nothing    -> return first
        Just rest  -> return (IATuple (first : rest))

parseUnaryNot :: Parser AST
parseUnaryNot = do
    _ <- lexeme $ try $ char '!' <* notFollowedBy (char '=')
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
    parseOrOp = keyword "ou" >> return (`IAInfix` "ou")

parseLogicalAnd :: Parser AST
parseLogicalAnd = chainl1' parseRelationnelle parseAndOp
  where
    parseAndOp = keyword "et" >> return (`IAInfix` "et")

parseRelationnelle :: Parser AST
parseRelationnelle = chainl1' parseAdditive parseRelationnelleOp
  where
    parseRelationnelleOp =
          try (symbol "=="  >> return (`IAInfix` "=="))
      <|> try (symbol "!=" >> return (`IAInfix` "!="))
      <|> try (symbol ">=" >> return (`IAInfix` ">="))
      <|> try (symbol "<=" >> return (`IAInfix` "<="))
      <|> (symbol "<"  >> return (`IAInfix` "<"))
      <|> (symbol ">"  >> return (`IAInfix` ">"))

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
parseBlock = many $ notFollowedBy (keyword "fin" <|> keyword "sinon") >> parseInstruction

parseInstruction :: Parser AST
parseInstruction =
        parseFunctionDef
    <|> parseMain
    <|> parseInclude
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
    _ <- keyword "fonction"
    name <- parseIdentifier

    _ <- symbol "("
    params <- parseFunctionArgument `sepBy` symbol ","
    _ <- symbol ")"

    _ <- symbol ":"
    returnTypeAnnot <- parseExplicitType

    body <- parseBlock
    _ <- keyword "fin"

    return (IAFunctionDef name params (Just returnTypeAnnot) body)

parseMain :: Parser AST
parseMain = do
    _ <- keyword "principal"
    args <- optional $ between (symbol "(") (symbol ")") $ do
        _ <- keyword "entier"
        countName <- parseIdentifier

        maybeList <- optional $ do
            _ <- symbol ","
            _ <- keyword "liste"
            _ <- symbol "<"
            _ <- keyword "phrase"
            _ <- symbol ">"
            parseIdentifier

        return (countName, maybeList)

    body <- parseBlock
    _ <- keyword "fin"

    case args of
        Nothing -> return (IAMain [] body)
        Just (c, Nothing) -> return (IAMain [c] body)
        Just (c, Just l)  -> return (IAMain [c, l] body)

parseInclude :: Parser AST
parseInclude = do
    void $ symbol "inclure"
    astString <- parseString
    case astString of
        IAString path -> return $ IAInclude path
        _             -> fail "Chemin d'inclusion invalide"
parseDeclaration :: Parser AST
parseDeclaration = do
    _ <- keyword "constante" <|> keyword "variable"
    typeAnnot <- parseExplicitType
    name <- parseIdentifier
    _ <- symbol "="
    IADeclare name (Just typeAnnot) <$> parseExpression

parseAssignment :: Parser AST
parseAssignment = do
    name <- parseIdentifier
    _ <- symbol "="
    IAAssign name <$> parseExpression

parseReturn :: Parser AST
parseReturn = do
    _ <- keyword "retourner"
    IAReturn <$> parseExpression

parseElseClause :: Parser (Maybe [AST])
parseElseClause = optional $ do
    maybeElseIf <- optional (try parseElseIf)
    case maybeElseIf of
        Just elseIf -> return [elseIf]

        Nothing -> do
            _ <- keyword "sinon"
            parseBlock

parseElseIf :: Parser AST
parseElseIf = do
    _ <- keyword "sinon"
    _ <- keyword "si"
    _ <- symbol "("
    cond <- parseExpression
    _ <- symbol ")"
    bodyThen <- parseBlock
    IAIf cond bodyThen <$> parseElseClause

parseConditional :: Parser AST
parseConditional = do
    _ <- keyword "si"
    _ <- symbol "("
    cond <- parseExpression
    _ <- symbol ")"

    bodyThen <- parseBlock

    elseBody <- parseElseClause

    _ <- keyword "fin"
    return (IAIf cond bodyThen elseBody)

parseWhile :: Parser AST
parseWhile = do
    _ <- keyword "tantque"
    _ <- symbol "("
    cond <- parseExpression
    _ <- symbol ")"
    body <- parseBlock
    _ <- keyword "fin"
    return (IAWhile cond body)

parseFor :: Parser AST
parseFor = do
    _ <- keyword "pour"
    _ <- symbol "("

    initExpr <- parseDeclaration <|> parseAssignment <|> return IAUnit
    _ <- symbol ";"

    condExpr <- parseExpression
    _ <- symbol ";"

    incExpr <- try parseAssignment <|> parseExpression
    _ <- symbol ")"

    body <- parseBlock
    _ <- keyword "fin"

    return (IAFor initExpr condExpr incExpr body)

-- ====================================================================
-- Point d'entrée du Programme CLaD
-- ====================================================================

parseProgramAST :: Parser AST
parseProgramAST = between sc eof $ do
    instructions <- many parseInstruction
    return (IAProgram instructions)