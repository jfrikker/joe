module Types where

import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Constant as C

type DataType = String

isIntType :: DataType -> Bool
isIntType "I32" = True
isIntType "I64" = True
isIntType _ = False

toLLVMType :: DataType -> T.Type
toLLVMType "I32" = T.i32
toLLVMType "I64" = T.i64

constantInt :: DataType -> Integer -> C.Constant
constantInt "I32" = C.Int 32
constantInt "I64" = C.Int 64