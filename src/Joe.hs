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

type CompilerM a = Except.Except Error.CompileError a

type IRM a = IR.IRBuilderT (Except.Except Error.CompileError) a

compile :: [AST.TopLevel] -> Either Error.CompileError LLVM.Module
compile tl = Except.runExcept $ do
  mainDef <- compileFunction "main" mainBody
  return LLVM.defaultModule {
    LLVM.moduleName = "in.j",
    LLVM.moduleDefinitions = [mainDef]
  }
  where (Just (AST.TopLevel _ _ mainBody)) = List.find (\(AST.TopLevel _ n _) -> n == "main") tl

compileFunction :: String -> [AST.PartialDefinition] -> CompilerM LLVM.Definition
compileFunction name [AST.Body expr] = do
  blocks <- IR.execIRBuilderT IR.emptyIRBuilder $ do
    (_, body) <- expressionToLlvm expr
    IR.ret body
  return $ LLVM.GlobalDefinition $ LLVM.functionDefaults {
    G.name = fromString name,
    G.returnType = T.i32,
    G.basicBlocks = blocks
  }

expressionToLlvm :: AST.Expression -> IRM (Types.DataType, LLVM.Operand)
expressionToLlvm (AST.Add op1 op2) = do
  (ty1, op1') <- expressionToLlvm op1
  (ty2, op2') <- expressionToLlvm op2
  unless (ty1 == ty2) $ Except.throwError $  Error.TypeError ty1 ty2
  res <- IR.add op1' op2'
  return (ty1, res)

expressionToLlvm (AST.I32Literal num) = return (Types.I32, IR.int32 num)
expressionToLlvm (AST.I64Literal num) = return (Types.I64, IR.int64 num)

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