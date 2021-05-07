module Parser where

import Text.Parsec (chainl1, choice, many, many1, optionMaybe, (<|>), skipMany, skipMany1, try)
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
typeSignature = function
  where function = do
          arg <- named
          ret <- optionMaybe $ try $ do
            ws
            string "->"
            ws
            function
          return $ maybe arg (AST.FunctionType arg) ret
        named = AST.NamedType <$> typeName

topLevel :: Parser AST.TopLevel
topLevel = do
  name <- identifier
  ws
  string "::"
  ws
  ts <- typeSignature
  newline
  options <- many1 $ do
    string name
    ws
    args <- many $ do
      ret <- identifier
      ws
      return ret
    char '='
    ws
    body <- expression
    newline
    return $ AST.PartialDefinition args body
  return $ AST.TopLevel ts name options

mod :: Parser [AST.TopLevel]
mod = do
  res <- many $ line >> topLevel
  line
  return res

expression :: Parser AST.Expression 
expression = expressionAdd expressionAtom

expressionAdd :: Parser AST.Expression -> Parser AST.Expression
expressionAdd next = chainl1 next $ do
  ws
  char '+'
  ws
  return AST.Add

expressionAtom :: Parser AST.Expression 
expressionAtom = ref <|> intLiteral
  where ref = do
          name <- identifier
          return $ AST.Reference name
        intLiteral = do
          num <- many1 digit
          return $ AST.IntLiteral $ read num