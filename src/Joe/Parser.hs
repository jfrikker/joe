{-# LANGUAGE FlexibleContexts #-}

module Joe.Parser where

import Joe.Parser.Expression (expression)
import Joe.Parser.TypeSignature (typeSignature)
import Joe.Parser.Util (emptyLines, identifier, literalOp)
import Text.Parsec (Stream, ParsecT, char, many, many1, newline, (<|>), skipMany1, string)

import qualified Joe.AST as AST

ws :: Stream s m Char => ParsecT s u m ()
ws = skipMany1 $ char ' '

topLevel :: Stream s m Char => ParsecT s u m AST.TopLevel
topLevel = do
  name <- identifier
  literalOp "::"
  ts <- typeSignature
  newline
  -- options <- many1 $ do
  --   string name
  --   ws
  --   def <- partialDefinition
  --   newline
  --   return def
  option <- do
    string name
    ws
    def <- partialDefinition
    newline
    return def
  emptyLines
  return $ AST.GlobalBinding $ AST.Binding name ts option

partialDefinition :: Stream s m Char => ParsecT s u m AST.PartialDefinition
partialDefinition = do
  args <- many1 match
  literalOp "="
  AST.PartialDefinition args <$> expression

match :: Stream s m Char => ParsecT s u m AST.Match
match = AST.Unbound <$> identifier

mod :: Stream s m Char => ParsecT s u m [AST.TopLevel]
mod = many $ emptyLines >> topLevel