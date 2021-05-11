module Joe.AST where

type Module = [TopLevel]

data TopLevel = TopLevel TypeSignature String [PartialDefinition] deriving Show

data PartialDefinition = FunctionArgument String PartialDefinition |
  Body Expression deriving Show

data TypeSignature = 
  NamedType String |
  FunctionType TypeSignature TypeSignature deriving Show

data Expression =
  Add Expression Expression |
  Call Expression Expression |
  I32Literal Integer |
  I64Literal Integer |
  Reference String deriving Show