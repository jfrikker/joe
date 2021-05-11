{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Joe where

import Control.Monad (unless)
import qualified Control.Monad.Except as Except
import qualified Data.List as List
import Data.String (fromString)
import qualified Joe.AST as AST
import qualified Joe.Error as Error
import qualified Joe.Scope as Scope
import qualified Joe.Types as Types
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder as IR

type CompilerM a = Scope.ScopeT (Except.Except Error.CompileError) a

compile :: [AST.TopLevel] -> Either Error.CompileError LLVM.Module
compile tl = return LLVM.defaultModule {
    LLVM.moduleName = "in.j",
    LLVM.moduleDefinitions = [compileFunction "main" mainBody]
  }
  where (Just (AST.TopLevel _ _ mainBody)) = List.find (\(AST.TopLevel _ n _) -> n == "main") tl

compileFunction :: String -> [AST.PartialDefinition] -> LLVM.Definition
compileFunction name [AST.Body expr] = LLVM.GlobalDefinition $ LLVM.functionDefaults {
  G.name = fromString name,
  G.returnType = T.i32,
  G.basicBlocks = IR.execIRBuilder IR.emptyIRBuilder $ expressionToLlvm expr >>= IR.ret
}

expressionToLlvm :: AST.Expression -> IR.IRBuilder LLVM.Operand
expressionToLlvm (AST.I32Literal num) = return $ IR.int32 num

-- data CompileError = TypeError Types.DataType Types.DataType deriving Show

-- typeOf :: (Scope.MonadScope m, Except.MonadError CompileError m) => AST.Expression -> m Types.DataType
-- typeOf (AST.Add left right) = do
--   leftType <- typeOf left
--   rightType <- typeOf right
--   unless (leftType == rightType) $ Except.throwError $ TypeError leftType rightType
--   return leftType
-- typeOf (AST.Call func arg) = do
--   fType <- typeOf func
--   argType <- typeOf arg
--   case fType of Types.Function a2 rest -> if argType == a2 then return rest else Except.throwError $ TypeError (Types.Function argType Types.Unknown) fType
--                 _ -> Except.throwError $ TypeError (Types.Function argType Types.Unknown) fType
-- typeOf (AST.I32Literal _) = return Types.I32
-- typeOf (AST.I64Literal _) = return Types.I64
-- typeOf (AST.Reference name) = Scope.resolveReferenceType name