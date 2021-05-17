{-# LANGUAGE FlexibleContexts #-}

module Joe.Parser where

import Joe.Parser.Expression (expression)
import Joe.Parser.TypeSignature (typeSignature)
import Joe.Parser.Util (emptyLines, identifier, lexeme, literalOp)
import Text.Parsec (Stream, ParsecT, char, many, many1, newline, (<|>), string)

import qualified Joe.AST as AST
import qualified Debug.Trace as Debug

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
    lexeme $ string name
    def <- partialDefinition
    newline
    return def
  emptyLines
  return $ Debug.traceShowId $ AST.GlobalBinding $ AST.Binding name ts option

partialDefinition :: Stream s m Char => ParsecT s u m AST.PartialDefinition
partialDefinition = do
  args <- many match
  literalOp "="
  AST.PartialDefinition args <$> expression

match :: Stream s m Char => ParsecT s u m AST.Match
match = identifier

mod :: Stream s m Char => ParsecT s u m [AST.TopLevel]
mod = many $ emptyLines >> topLevel