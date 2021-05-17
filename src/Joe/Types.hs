module Joe.Types where

import qualified LLVM.AST as LLVM

data DataType = 
  I32 |
  I64 |
  Constant DataType |
  Function DataType DataType deriving (Show, Eq, Ord)

data Value = Value DataType LLVM.Operand