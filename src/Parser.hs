module Parser where

import Text.Parsec (choice, many, many1, (<|>), skipMany, skipMany1, try)
import Text.Parsec.Text.Lazy (Parser)
import Text.ParserCombinators.Parsec.Char (alphaNum, char, digit, lower, newline, string, upper)

import qualified AST

ws :: Parser ()
ws = skipMany1 $ char ' '

optWs :: Parser ()
optWs = skipMany $ char ' '

line :: Parser ()
line = skipMany $ do
  optWs
  newline

identifier :: Parser String
identifier = do
  first <- lower
  rest <- many alphaNum
  return $ first : rest

typeName :: Parser String
typeName = do
  first <- upper
  rest <- many alphaNum
  return $ first : rest

typeSignature :: Parser AST.TypeSignature
typeSignature = named
  where named = AST.TypeNamed <$> typeName

topLevel :: Parser AST.TopLevel
topLevel = do
  name <- identifier
  ws
  string "::"
  ws
  ts <- typeSignature
  line
  string name
  ws
  char '='
  ws
  body <- expression
  return $ AST.TopLevel ts name body

mod :: Parser [AST.TopLevel]
mod = do
  res <- many $ line >> topLevel
  line
  return res

expression :: Parser AST.Expression 
expression = choice [try add, try atom]
  where atom = do
          num <- many1 digit
          return $ AST.IntLiteral $ read num
        add = do
          first <- atom
          ws
          char '+'
          ws
          rest <- expression
          return $ AST.Add first rest