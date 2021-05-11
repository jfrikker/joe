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

type CompilerM a = State.StateT CompilerState (Except.ExceptT Error.CompileError (Reader.Reader AST.Module)) a

type IRM a = IR.IRBuilderT (State.StateT CompilerState (Except.ExceptT Error.CompileError (Reader.Reader AST.Module))) a

data CompilerState = CompilerState {
  definitions :: [LLVM.Definition],
  symbols :: Map.Map (String, [Types.DataType]) (Types.DataType, LLVM.Operand),
  symbSequence :: Int
}

emptyState :: CompilerState
emptyState = CompilerState {
  definitions = [],
  symbols = Map.empty,
  symbSequence = 0
}

compile :: AST.Module -> Either Error.CompileError LLVM.Module
compile = Reader.runReader $ Except.runExceptT $ flip State.evalStateT emptyState $ do
  compileFunction ("main", [])
  defs <- State.gets definitions
  return LLVM.defaultModule {
    LLVM.moduleName = "in.j",
    LLVM.moduleDefinitions = defs
  }

findTree :: String -> CompilerM AST.TopLevel
findTree name = do
  binding <- Reader.asks $ List.find (\(AST.TopLevel _ n _) -> n == name)
  case binding of
    Just tree -> return tree
    Nothing -> Except.throwError $ Error.UnknownBinding name

generateName :: String -> CompilerM String
generateName name = do
  thisSeq <- State.state $ \s -> (symbSequence s, s {
    symbSequence = symbSequence s + 1
  })
  return $ name ++ "_" ++ show thisSeq

findCompiledFunction :: (String, [Types.DataType]) -> CompilerM (Maybe (Types.DataType, LLVM.Operand))
findCompiledFunction sig = State.gets $ Map.lookup sig . symbols

addDefinition :: LLVM.Definition -> CompilerM ()
addDefinition def = State.modify $ \s -> s {
  definitions = def : definitions s
}

addCompiledFunction :: (String, [Types.DataType]) -> (Types.DataType, LLVM.Operand) -> CompilerM ()
addCompiledFunction sig def = State.modify $ \s -> s {
  symbols = Map.insert sig def $ symbols s
}

compileFunction :: (String, [Types.DataType]) -> CompilerM (Types.DataType, LLVM.Operand)
compileFunction sig = do
  existing <- findCompiledFunction sig
  case existing of
    Just symb -> return symb
    Nothing -> do
      blah <- findTree $ fst sig
      let (AST.TopLevel _ _ [AST.Body expr]) = blah
      (retTy, blocks) <- IR.runIRBuilderT IR.emptyIRBuilder $ do
        (ty, body) <- expressionToLlvm expr
        res <- IR.ret body
        return ty
      name <- generateName $ fst sig
      addDefinition $ LLVM.GlobalDefinition $ LLVM.functionDefaults {
        G.name = fromString name,
        G.returnType = T.i32,
        G.basicBlocks = blocks
      }
      let operand = LLVM.ConstantOperand  $ C.GlobalReference (T.FunctionType T.i32 [] False) (fromString name)
      let res = (retTy, operand)
      addCompiledFunction sig res
      return res

expressionToLlvm :: AST.Expression -> IRM (Types.DataType, LLVM.Operand)
expressionToLlvm (AST.Add op1 op2) = do
  (ty1, op1') <- expressionToLlvm op1
  (ty2, op2') <- expressionToLlvm op2
  unless (ty1 == ty2) $ Except.throwError $  Error.TypeError ty1 ty2
  res <- IR.add op1' op2'
  return (ty1, res)
expressionToLlvm (AST.I32Literal num) = return (Types.I32, IR.int32 num)
expressionToLlvm (AST.I64Literal num) = return (Types.I64, IR.int64 num)
expressionToLlvm (AST.Reference name) = do
  (ty, func) <- lift $ compileFunction (name, [])
  res <- IR.call func []
  return (ty, res)

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