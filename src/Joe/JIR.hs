{-# LANGUAGE FlexibleContexts #-}

module Joe.JIR where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Joe.AST as AST
import qualified Joe.Error as Error
import qualified LLVM.Prelude as LLVM

type Symbol = LLVM.ShortByteString

newtype JIRState = JIRState {
  symbols :: Map.Map BindingSignature Binding
}

emptyState :: JIRState
emptyState = JIRState {
  symbols = Map.empty
}

data BindingSignature = BindingSignature {
  bindingName :: String,
  argTypes :: [ConcreteTypeSignature],
  curryed :: Int
} deriving Show

data Binding = Binding {
  bindingSymbol :: Symbol,
  bindingClosureType :: Maybe DataType,
  argType :: Maybe DataType,
  returnType :: DataType,
  body :: Expression
} deriving Show

data ConcreteTypeSignature =
  Named String |
  Function DataType DataType deriving Show

data DataType = I32 |
  I64 |
  Closure DataType [DataType] [DataType] deriving Show

data Expression = Expression deriving Show

inputClosureType :: 

buildIR :: (Monad m, Except.MonadError Error.CompileError m) => AST.Module -> m [Binding]
buildIR mod = do
  s <- flip Reader.runReaderT mod $ State.execStateT (symbolForFunction main) emptyState
  return $ bindings s
  where main = BindingSignature {
    bindingName = "main",
    argTypes = [],
    curryed = 0
  }

computeBinding :: (Monad m, Except.MonadError Error.CompileError m, State.MonadState JIRState m, Reader.MonadReader AST.Module m) => BindingSignature -> (Symbol -> m ()) -> m Symbol
computeBinding sig f = do
  existing <- State.gets $ Map.lookup sig . symbolCache
  case existing of
    Just e -> return e
    Nothing -> do
      symb <- generateSymbol $ bindingName sig
      State.modify $ \s -> s { symbols = Map.insert sig symb $ symbolCache s}
      f symb
      return symb

realizeBinding :: (Monad m, Except.MonadError Error.CompileError m, State.MonadState JIRState m, Reader.MonadReader AST.Module m) => BindingSignature -> m Binding
realizeBinding sig = do
  existing <- State.gets $ Map.lookup sig . symbolCache
  case existing of
    Just e -> return e
    Nothing -> do
      astBinding <- Reader.asks $ AST.findBinding $ bindingName sig
      case astBinding of
        Nothing -> Except.throwError $ Error.UnknownBinding $ bindingName sig
        Just (AST.Binding _ astType (AST.PartialDefinition args body)) -> do
          validateTypeSignature astType (argTypes sig)
          symb <- generateSymbol $ bindingName sig
          let needsClosure = curryed sig > 0
          realizeFunction symb 
      let 
      State.modify $ \s -> s { symbols = Map.insert sig symb $ symbolCache s}
      f symb
      return symb
  
  
  computeSymbol sig $ \symb -> do
  astBinding <- Reader.asks $ AST.findBinding $ bindingName sig
  case astBinding of
    Nothing -> Except.throwError $ Error.UnknownBinding $ bindingName sig
    Just (AST.Binding _ astType astDef) -> do
      validateTypeSignature astType (argTypes sig)
      realizeFunction symb 

realizeFunction :: (Monad m, Except.MonadError Error.CompileError m, State.MonadState JIRState m, Reader.MonadReader AST.Module m) => BindingSignature -> [AST.PartialDefinition] -> m Symbol