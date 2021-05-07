{-# LANGUAGE FlexibleContexts #-}

module Joe.Parser.Util(
  emptyLines,
  identifier,
  lexeme,
  literalIdentifier,
  literalOp,
  operator,
  typeIdentifier
) where

import Control.Monad (unless)
import Text.Parsec(Stream, ParsecT, alphaNum, char, many, many1, lower, newline, oneOf, skipMany, upper)

optWs :: Stream s m Char => ParsecT s u m ()
optWs = skipMany $ char ' '

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = do
  res <- p
  optWs
  return res

operator :: Stream s m Char => ParsecT s u m String
operator = lexeme $ many1 $ oneOf ":!#$%&*+./<=>?@\\^|-~"

literalOp :: Stream s m Char => String -> ParsecT s u m ()
literalOp expected = do
  op <- operator
  unless (op == expected) $ fail expected

identifier :: Stream s m Char => ParsecT s u m String
identifier = lexeme $ do
  first <- lower
  rest <- many alphaNum
  return $ first : rest

literalIdentifier :: Stream s m Char => String -> ParsecT s u m ()
literalIdentifier expected = do
  id <- identifier
  unless (id == expected) $ fail expected

typeIdentifier :: Stream s m Char => ParsecT s u m String
typeIdentifier = lexeme $ do
  first <- upper
  rest <- many alphaNum
  return $ first : rest

emptyLines :: Stream s m Char => ParsecT s u m ()
emptyLines = skipMany $ do
  optWs
  newline