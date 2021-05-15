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
import qualified Joe.Scope as Scope
import qualified Joe.Types as Types
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder as IR

type CompilerM a = Scope.ScopeT (State.StateT CompilerState (Except.Except Error.CompileError)) a

type IRM a = IR.IRBuilderT (Scope.ScopeT (State.StateT CompilerState (Except.Except Error.CompileError))) a

data CompilerState = CompilerState {
  definitions :: [LLVM.Definition],
  symbols :: Map.Map (String, Int) LLVM.Operand,
  symbSequence :: Int
}

emptyState :: CompilerState
emptyState = CompilerState {
  definitions = [],
  symbols= Map.empty,
  symbSequence = 0
}

compile :: AST.Module -> Either Error.CompileError LLVM.Module
compile mod = Except.runExcept $ flip State.evalStateT emptyState $ Scope.runScopeT scope $ do
  compileFunction "main" 0
  defs <- State.gets definitions
  return LLVM.defaultModule {
    LLVM.moduleName = "in.j",
    LLVM.moduleDefinitions = defs
  }
  where scope = Scope.globalScope $ AST.topLevelBindings mod 

generateName :: String -> CompilerM String
generateName name = do
  thisSeq <- State.state $ \s -> (symbSequence s, s {
    symbSequence = symbSequence s + 1
  })
  return $ name ++ "_" ++ show thisSeq

findCompiledFunction :: (String, Int) -> CompilerM (Maybe LLVM.Operand)
findCompiledFunction sig = State.gets $ Map.lookup sig . symbols

addDefinition :: LLVM.Definition -> CompilerM ()
addDefinition def = State.modify $ \s -> s {
  definitions = def : definitions s
}

addCompiledFunction :: (String, Int) -> LLVM.Operand -> CompilerM ()
addCompiledFunction sig def = State.modify $ \s -> s {
  symbols = Map.insert sig def $ symbols s
}

countArguments :: [AST.PartialDefinition] -> CompilerM Int
countArguments [def] = return $ AST.partialDefinitionArguments def
countArguments (p : rest) = do
  r <- countArguments rest
  let c = AST.partialDefinitionArguments p
  unless (r == c) $ Except.throwError Error.MismatchedArguments
  return c


compileFunction :: String -> Int -> CompilerM LLVM.Operand
compileFunction name args = do
  existing <- findCompiledFunction (name, args)
  case existing of
    Just symb -> return symb
    Nothing -> do
      (AST.Binding _ overallTy [blah]) <- Scope.resolveBinding name
      overallTy' <- Scope.resolveType overallTy
      let (funcTy, body) = removeArgs args overallTy' blah
      symb <- case (funcTy, body) of
        (Types.Function argTy retTy, AST.FunctionArgument argName body') -> compileOneArgFunction name argTy retTy argName body'
        (funcTy', AST.Body body') -> compileNoArgFunction name funcTy' body'
      addCompiledFunction (name, args) symb
      return symb

removeArgs :: Int -> Types.DataType -> AST.PartialDefinition -> (Types.DataType, AST.PartialDefinition)
removeArgs 0 ty def = (ty, def)
removeArgs i (Types.Function _ ty) (AST.FunctionArgument _ def) = removeArgs (i - 1) ty def

compileNoArgFunction :: String -> Types.DataType -> AST.Expression -> CompilerM LLVM.Operand 
compileNoArgFunction name retTy body = do
  (bodyTy, blocks) <- IR.runIRBuilderT IR.emptyIRBuilder $ do
    res <- expressionToLlvm body
    IR.ret $ Types.valueOperand res
    return $ Types.valueType res
  unless (retTy == bodyTy) $ Except.throwError $ Error.TypeError retTy bodyTy
  let retTyLlvm = Types.llvmType retTy
  addDefinition $ LLVM.GlobalDefinition $ LLVM.functionDefaults {
    G.name = fromString name,
    G.parameters = ([], False),
    G.returnType = retTyLlvm,
    G.basicBlocks = blocks
  }
  return $ LLVM.ConstantOperand $ C.GlobalReference (T.FunctionType retTyLlvm [] False) (fromString name)

compileOneArgFunction :: String -> Types.DataType -> Types.DataType -> String -> AST.Expression -> CompilerM LLVM.Operand 
compileOneArgFunction name argTy retTy argName body = Scope.withOperand argName (Types.LValue argTy $ LLVM.LocalReference (Types.llvmType argTy) "addr") $ do
  (bodyTy, blocks) <- IR.runIRBuilderT IR.emptyIRBuilder $ do
    res <- expressionToLlvm body
    IR.ret $ Types.valueOperand res
    return $ Types.valueType res
  unless (retTy == bodyTy) $ Except.throwError $ Error.TypeError retTy bodyTy
  let argTyLlvm = Types.llvmType argTy
  let retTyLlvm = Types.llvmType retTy
  addDefinition $ LLVM.GlobalDefinition $ LLVM.functionDefaults {
    G.name = fromString name,
    G.parameters = ([G.Parameter argTyLlvm (fromString name) []], False),
    G.returnType = retTyLlvm,
    G.basicBlocks = blocks
  }
  return $ LLVM.ConstantOperand $ C.GlobalReference (T.FunctionType retTyLlvm [argTyLlvm] False) (fromString name)

expressionToLlvm :: AST.Expression -> IRM Types.Value
expressionToLlvm (AST.Add op1 op2) = do
  v1 <- expressionToLlvm op1
  v2 <- expressionToLlvm op2
  unless (Types.valueType v1 == Types.valueType v2) $ Except.throwError $ Error.TypeError (Types.valueType v1) (Types.valueType v2)
  res <- IR.add (Types.valueOperand v1) (Types.valueOperand v2)
  return $ Types.LValue (Types.valueType v1) res
-- expressionToLlvm (AST.Call func arg) = do
--   func' <- expressionToLlvm func
--   case func' of
--     LValue _ _ -> Except.throwError Error.ExpectedFunction
--     Function _ name args funcOperand -> do
--       arg' <- expressionToLlvm arg
--       (resTy, funcRef) <- lift $ compileFunction name (args ++ [valueType arg'])
--       res <- IR.call funcRef [(valueOperand arg', [])]
--       return (resTy, res)
expressionToLlvm (AST.I32Literal num) = return $ Types.LValue Types.I32 $ IR.int32 num
expressionToLlvm (AST.I64Literal num) = return $ Types.LValue Types.I64 $ IR.int64 num
expressionToLlvm (AST.Reference name) = do
  (AST.Binding _ bindTy _) <- lift $ Scope.resolveBinding name
  bindTy' <- lift $ Scope.resolveType bindTy
  func <- lift $ compileFunction name 0
  res <- IR.call func []
  return $ Types.LValue bindTy' res

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