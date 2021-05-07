{-# LANGUAGE FlexibleContexts #-}

module Joe.Parser.Expression (
  expression
) where

import qualified Joe.AST as AST
import Joe.Parser.Util (identifier, literalOp, lexeme)
import Text.Parsec(Stream, ParsecT, (<|>), chainl1, digit, many1)

expression :: Stream s m Char => ParsecT s u m AST.Expression
expression = expressionAdd expressionAtom

expressionAdd :: Stream s m Char => ParsecT s u m AST.Expression -> ParsecT s u m AST.Expression
expressionAdd next = chainl1 next $ do
  literalOp "+"
  return AST.Add

expressionAtom :: Stream s m Char => ParsecT s u m AST.Expression 
expressionAtom = ref <|> intLiteral
  where ref = AST.Reference <$> identifier
        intLiteral = do
          num <- lexeme $ many1 digit
          return $ AST.IntLiteral $ read num