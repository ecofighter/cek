{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Functor
import Data.Set qualified as Set
import Syntax
import Text.Trifecta

reserved :: ByteString -> Bool
reserved s = Set.member s set
  where
    set =
      Set.fromList
        [ "let",
          "rec",
          "in",
          "fun",
          "if",
          "then",
          "else",
          "true",
          "false"
        ]

parseExp :: Parser Exp
parseExp = choice [parseLet, parseFun, parseIf, try parseApp, parseAtomicExp] <?> "Expression"

parseAtomicExp :: Parser Exp
parseAtomicExp = choice [parseVar, parseInt, parseBool, parens parseExp] <?> "Atomic Expression"

parseVar :: Parser Exp
parseVar = Var <$> parseIdent <?> "Variable"

parseInt :: Parser Exp
parseInt = Int <$> integer <?> "Integer value"

parseBool :: Parser Exp
parseBool = Bool <$> choice [symbol "true" $> True, symbol "false" $> False] <?> "Bool value"

parseFun :: Parser Exp
parseFun = Fun <$ symbol "fun" <*> some param <* symbol "->" <*> parseExp <?> "Function"
  where
    param = choice [annotated, fmap (,) parseIdent <*> pure Nothing]
    annotated = parens $ do
      idnt <- parseIdent
      _ <- symbolic ':'
      ty <- parseTy
      pure (idnt, Just ty)

parseApp :: Parser Exp
parseApp = App <$> parseAtomicExp <*> some parseAtomicExp <?> "Application"

parseLet :: Parser Exp
parseLet =
  Let
    <$ symbol "let"
    <*> parseIdent
    <*> tyAnnot
    <* symbolic '='
    <*> parseExp
    <* symbol "in"
    <*> parseExp <?> "Let Expression"
  where
    tyAnnot = optional (symbolic ':' *> parseTy)

parseIf :: Parser Exp
parseIf =
  If
    <$ symbol "if"
    <*> parseExp
    <* symbol "then"
    <*> parseExp
    <* symbol "else"
    <*> parseExp <?> "If Expression"

parseIdent :: Parser ByteString
parseIdent = try (token aux) <?> "Identifier"
  where
    aux = do
      res <- sliced $ fmap (:) lower <*> many alphaNum
      if reserved res
        then
          fail "reserved"
        else
          pure res

parseTy :: Parser Ty
parseTy = choice [parseTyArrow, parseAtomicTy] <?> "Type"

parseAtomicTy :: Parser Ty
parseAtomicTy = choice [parseTyInt, parseTyBool, parens parseTy] <?> "Atomic Type"

parseTyInt :: Parser Ty
parseTyInt = symbol "Int" $> TyInt <?> "Type Int"

parseTyBool :: Parser Ty
parseTyBool = symbol "Bool" $> TyBool <?> "Type Bool"

parseTyArrow :: Parser Ty
parseTyArrow = parseAtomicTy `chainr1` arr <?> "Type Arrow"
  where
    arr = TyArr <$ symbol "->"
