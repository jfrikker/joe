{-# LANGUAGE FlexibleContexts #-}

module Joe.Parser.TypeSignature (
  typeSignature
) where

import qualified Joe.AST as AST
import Joe.Parser.Util (literalOp, typeIdentifier)
import Text.Parsec(Stream, ParsecT, chainr1)

named :: Stream s m Char => ParsecT s u m AST.TypeSignature
named = AST.NamedType <$> typeIdentifier

function :: Stream s m Char => ParsecT s u m AST.TypeSignature -> ParsecT s u m AST.TypeSignature
function next = chainr1 next $ do
  literalOp "->"
  return AST.FunctionType

typeSignature :: Stream s m Char => ParsecT s u m AST.TypeSignature
typeSignature = function named