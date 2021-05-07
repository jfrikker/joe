module Joe.Types where

data DataType = 
  Function DataType DataType |
  I32 |
  I64 |
  Unknown deriving (Show, Eq)