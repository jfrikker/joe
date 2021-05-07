module Joe.AST where

data TopLevel = TopLevel TypeSignature String [PartialDefinition] deriving Show

data PartialDefinition = FunctionArgument String PartialDefinition |
  Body Expression deriving Show

data TypeSignature = 
  NamedType String |
  FunctionType TypeSignature TypeSignature deriving Show

data Expression =
  Call Expression Expression |
  Add Expression Expression |
  Reference String |
  I32Literal Integer |
  I64Literal Integer deriving Show
