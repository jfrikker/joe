module AST (
  TopLevel(..),
  TypeSignature(..),
  Expression(..),
) where

data TopLevel = TopLevel TypeSignature String Expression deriving Show

data TypeSignature = 
  TypeNamed String deriving Show

data Expression =
  Add Expression Expression |
  IntLiteral Integer deriving Show