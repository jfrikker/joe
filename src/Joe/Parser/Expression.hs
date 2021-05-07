{-# LANGUAGE FlexibleContexts #-}

module Joe.Parser.Expression (
  expression
) where

import qualified Joe.AST as AST
import Joe.Parser.Util (identifier, literalOp, lexeme)
import Text.Parsec(Stream, ParsecT, (<|>), chainl1, choice, digit, many1, string, try)

expression :: Stream s m Char => ParsecT s u m AST.Expression
expression = call $ add atom

call :: Stream s m Char => ParsecT s u m AST.Expression -> ParsecT s u m AST.Expression
call next = chainl1 next $ return AST.Call

add :: Stream s m Char => ParsecT s u m AST.Expression -> ParsecT s u m AST.Expression
add next = chainl1 next $ do
  literalOp "+"
  return AST.Add

atom :: Stream s m Char => ParsecT s u m AST.Expression 
atom = ref <|> intLiteral
  where ref = AST.Reference <$> identifier
        intLiteral = lexeme $ do
          num <- many1 digit
          ty <- choice [intType "i32" AST.I32Literal, intType "i64" AST.I64Literal]
          return $ ty $ read num
        intType c ty = do
          try $ string c
          return ty