{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Joe where

import Control.Monad (unless)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.Trans (lift)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.String (fromString)
import qualified Joe.AST as AST
import qualified Joe.Error as Error
import qualified Joe.Types as Types
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder as IR

type CompilerM a = State.StateT CompilerState (Except.ExceptT Error.CompileError (Reader.Reader AST.Module)) a

type IRM a = IR.IRBuilderT (IR.ModuleBuilderT (State.StateT CompilerState (Except.ExceptT Error.CompileError (Reader.Reader AST.Module)))) a

type ModM a = IR.ModuleBuilderT (State.StateT CompilerState (Except.ExceptT Error.CompileError (Reader.Reader AST.Module))) a

data CompilerState = CompilerState {
  symbols :: Map.Map String Types.Value,
  symbSequence :: Word
}

emptyState :: CompilerState
emptyState = CompilerState {
  symbols= Map.empty,
  symbSequence = 0
}

compile :: AST.Module -> Either Error.CompileError LLVM.Module
compile mod = flip Reader.runReader mod $ Except.runExceptT $ flip State.evalStateT emptyState $ do
  defs <- IR.execModuleBuilderT IR.emptyModuleBuilder $ compileFunction "main"
  return LLVM.defaultModule {
    LLVM.moduleName = "in.j",
    LLVM.moduleDefinitions = defs
  }

generateName :: (Monad m, State.MonadState CompilerState m) => m LLVM.Name
generateName = do
  thisSeq <- State.state $ \s -> (symbSequence s, s {
    symbSequence = symbSequence s + 1
  })
  return $ LLVM.UnName thisSeq

findSymbol :: (State.MonadState CompilerState m) => String -> m (Maybe Types.Value)
findSymbol sig = State.gets $ Map.lookup sig . symbols

addSymbol :: (State.MonadState CompilerState m) => String -> Types.Value -> m ()
addSymbol sig def = State.modify $ \s -> s {
  symbols = Map.insert sig def $ symbols s
}

compileFunction :: String -> ModM Types.Value
compileFunction name = ifAbsent $ do
  (AST.Binding _ overallTy (AST.PartialDefinition argNames def)) <- findBinding
  let argTypes = map resolveAstType $ AST.typeSignatureArguments overallTy
  let returnType = resolveAstType $ AST.typeSignatureReturnType overallTy
  anonName <- generateName
  baseFunc <- IR.function anonName (map (\t -> (toLlvmType t, IR.NoParameterName)) argTypes) (toLlvmType returnType) $ \a -> do
    let a' = Map.fromList $ zip argNames $ zipWith Types.Value argTypes a
    Types.Value _ res <- expressionToLlvm a' def
    IR.ret res
  case length argTypes of
    0 -> return $ Types.Value (Types.Constant returnType) baseFunc
    1 -> return $ Types.Value (Types.Function (head argTypes) returnType) baseFunc
  where ifAbsent f = do
          existing <- findSymbol name
          maybe f return existing
        findBinding = do
          astBinding <- Reader.asks $ AST.findBinding name
          case astBinding of
            Nothing -> Except.throwError $ Error.UnknownBinding name
            Just b -> return b

expressionToLlvm :: Map.Map String Types.Value -> AST.Expression -> IRM Types.Value
expressionToLlvm scope (AST.Add op1 op2) = do
  (Types.Value v1ty v1op) <- expressionToLlvm scope op1
  (Types.Value v2ty v2op) <- expressionToLlvm scope op2
  unless (v1ty == v2ty) $ Except.throwError $ Error.TypeError v1ty v2ty
  res <- IR.add v1op v2op
  return $ Types.Value v1ty res
expressionToLlvm scope (AST.Call func arg) = do
  Types.Value funcTy funcOp <- expressionToLlvm scope func
  Types.Value argTy argOp <- expressionToLlvm scope arg
  case funcTy of
    Types.Constant retTy -> do
      res <- IR.call funcOp []
      return $ Types.Value retTy res
    Types.Function expArgTy retTy -> do
      unless (argTy == expArgTy) $ Except.throwError $ Error.TypeError argTy expArgTy
      res <- IR.call funcOp [(argOp, [])]
      return $ Types.Value retTy res
expressionToLlvm _ (AST.I32Literal num) = return $ Types.Value Types.I32 $ IR.int32 num
expressionToLlvm _ (AST.I64Literal num) = return $ Types.Value Types.I64 $ IR.int64 num
expressionToLlvm scope (AST.Reference name) = do
  let local = Map.lookup name scope
  case local of
    Just v -> return v
    Nothing -> do
      binding <- lift $ compileFunction name
      case binding of
        Types.Value (Types.Constant retTy) func -> do
          res <- IR.call func []
          return $ Types.Value retTy res
        _ -> return binding

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

resolveAstType :: AST.TypeSignature -> Types.DataType
resolveAstType (AST.NamedType "I32") = Types.I32

toLlvmType :: Types.DataType -> LLVM.Type
toLlvmType Types.I32  = T.i32