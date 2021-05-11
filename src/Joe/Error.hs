module Joe.Error where

import qualified Joe.Types as Types

data CompileError = TypeError Types.DataType Types.DataType |
  UnknownBinding String deriving Show