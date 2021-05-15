module Joe.Types where

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as T

data ConcreteTypeSignature =
  Named String |
  Function DataType DataType deriving (Show, Eq, Ord)

data DataType = 
  I32 |
  I64 |
  Unknown deriving (Show, Eq, Ord)

llvmType :: DataType -> T.Type 
llvmType I32 = T.i32 
llvmType I64 = T.i64

data Value = LValue DataType LLVM.Operand

valueType :: Value -> DataType
valueType (LValue t _) = t

valueOperand :: Value -> LLVM.Operand
valueOperand (LValue _ o) = o