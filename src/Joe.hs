{-# LANGUAGE FlexibleContexts #-}

module Joe where

import Control.Monad (unless)
import qualified Control.Monad.Except as Except
import qualified Joe.AST as AST
import qualified Joe.Scope as Scope
import qualified Joe.Types as Types

data CompileError = TypeError Types.DataType Types.DataType deriving Show

typeOf :: (Scope.MonadScope m, Except.MonadError CompileError m) => AST.Expression -> m Types.DataType
typeOf (AST.Add left right) = do
  leftType <- typeOf left
  rightType <- typeOf right
  unless (leftType == rightType) $ Except.throwError $ TypeError leftType rightType
  return leftType
typeOf (AST.Call func arg) = do
  fType <- typeOf func
  argType <- typeOf arg
  case fType of Types.Function a2 rest -> if argType == a2 then return rest else Except.throwError $ TypeError (Types.Function argType Types.Unknown) fType
                _ -> Except.throwError $ TypeError (Types.Function argType Types.Unknown) fType
typeOf (AST.I32Literal _) = return Types.I32
typeOf (AST.I64Literal _) = return Types.I64
typeOf (AST.Reference name) = Scope.resolveReferenceType name