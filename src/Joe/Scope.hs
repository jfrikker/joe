{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Joe.Scope where

import qualified Joe.AST as AST
import qualified Joe.Types as Types

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Scope = GlobalScope {
  datatypes :: Map.Map String Types.DataType,
  bindings :: Map.Map String Types.DataType
}

globalScope :: Scope
globalScope = GlobalScope {
  datatypes = Map.fromList [
    ("I32", Types.I32),
    ("I64", Types.I64)
  ]
}

-- resolveType :: Scope -> AST.TypeSignature -> Types.DataType
-- resolveType Scope {datatypes = dt} (AST.NamedType name) = fromJust $ Map.lookup name dt

class MonadScope s where
  resolveReferenceType :: String -> s a

newtype ScopeT m a = ScopeT {
  inner :: State.StateT Scope m a
  } deriving (Functor,
  Applicative,
  Monad,
  Trans.MonadTrans)

instance Except.MonadError e m => Except.MonadError e (ScopeT m) where
  throwError = Trans.lift . Except.throwError
  catchError (ScopeT s) f = ScopeT $ Except.catchError s $ inner . f

runScopeT :: Monad m => ScopeT m a -> Scope -> m a
runScopeT (ScopeT s) = State.evalStateT s

-- instance MonadScope (ScopeT m) where
--   resolveReferenceType name = do
