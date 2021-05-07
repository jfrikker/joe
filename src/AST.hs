module AST where

data TopLevel = TopLevel TypeSignature String [PartialDefinition] deriving Show

data PartialDefinition = PartialDefinition [String] Expression deriving Show

data TypeSignature = 
  NamedType String |
  FunctionType TypeSignature TypeSignature deriving Show

data Expression =
  Add Expression Expression |
  Reference String |
  IntLiteral Integer deriving Show