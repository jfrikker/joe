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
  options <- many1 $ do
    string name
    ws
    def <- partialDefinition
    newline
    return def
  emptyLines
  return $ AST.TopLevel ts name options

partialDefinition :: Stream s m Char => ParsecT s u m AST.PartialDefinition
partialDefinition = body <|> arg
  where body = do
          literalOp "="
          AST.Body <$> expression
        arg = do
          a <- identifier
          AST.FunctionArgument a <$> partialDefinition

mod :: Stream s m Char => ParsecT s u m [AST.TopLevel]
mod = many $ emptyLines >> topLevel