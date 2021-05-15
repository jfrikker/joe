module Joe.Error where

import qualified Joe.AST as AST
import qualified Joe.Types as Types

data CompileError = TypeError Types.DataType Types.DataType |
  UnknownBinding String |
  UnknownType AST.TypeSignature |
  MismatchedArguments |
  ExpectedFunction deriving Show