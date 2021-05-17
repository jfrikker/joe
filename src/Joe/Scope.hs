-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Joe.Scope where

-- import qualified Joe.AST as AST
-- import qualified Joe.Error as Error
-- import qualified Joe.Types as Types

-- import qualified Control.Monad.Except as Except
-- import qualified Control.Monad.State as State
-- import qualified Control.Monad.Trans as Trans
-- import qualified Data.List as List
-- import qualified Data.Map as Map
-- import qualified Data.Maybe as Maybe

-- data Scope = Scope {
--   parent :: Maybe Scope,
--   bindings :: [AST.Binding],
--   operands :: Map.Map String Types.Value 
-- }

-- globalScope :: [AST.Binding] -> Scope
-- globalScope b = Scope {
--   parent = Nothing,
--   bindings = b,
--   operands = Map.empty
-- }

-- -- resolveType :: Scope -> AST.TypeSignature -> Types.DataType
-- -- resolveType Scope {datatypes = dt} (AST.NamedType name) = fromJust $ Map.lookup name dt

-- newtype ScopeT m a = ScopeT {
--   inner :: State.StateT Scope m a
--   } deriving (Functor,
--   Applicative,
--   Monad,
--   Trans.MonadTrans)

-- instance Except.MonadError e m => Except.MonadError e (ScopeT m) where
--   throwError = Trans.lift . Except.throwError
--   catchError (ScopeT s) f = ScopeT $ Except.catchError s $ inner . f

-- runScopeT :: Monad m => Scope -> ScopeT m a -> m a
-- runScopeT i (ScopeT s) = State.evalStateT s i

-- resolveBinding :: Except.MonadError Error.CompileError m => String -> ScopeT m AST.Binding
-- resolveBinding name = ScopeT $ do
--   b <- State.gets $ resolveBinding' name
--   case b of
--     Just b -> return b
--     Nothing -> Except.throwError $ Error.UnknownBinding name

-- resolveBinding' :: String -> Scope -> Maybe AST.Binding
-- resolveBinding' name s = Maybe.listToMaybe $ Maybe.catMaybes [shallow, p]
--   where shallow = findBindingShallow name s
--         p = parent s >>= resolveBinding' name

-- findBindingShallow :: String -> Scope -> Maybe AST.Binding
-- findBindingShallow name = List.find (\(AST.Binding n _ _) -> n == name) . bindings

-- resolveType :: Except.MonadError Error.CompileError m => AST.TypeSignature -> ScopeT m Types.DataType
-- resolveType (AST.NamedType "I32") = return Types.I32
-- resolveType (AST.NamedType "I64") = return Types.I64
-- resolveType (AST.FunctionType arg ret) = do
--   arg' <- resolveType arg
--   ret' <- resolveType ret
--   return $ Types.Function arg' ret'
-- resolveType ty = Except.throwError $ Error.UnknownType ty

-- findOperand :: (Monad m) => String -> ScopeT m (Maybe Types.Value)
-- findOperand k = ScopeT $ State.gets $ Map.lookup k . operands

-- addOperand :: (Monad m) => String -> Types.Value -> ScopeT m ()
-- addOperand k v = ScopeT $ State.modify $ \s -> s {
--     operands = Map.insert k v $ operands s
--   }

-- withOperand :: Monad m => String -> Types.Value -> ScopeT m a -> ScopeT m a
-- withOperand k v (ScopeT op) = ScopeT $ do
--   p <- State.get
--   State.put Scope {
--     parent = Just p,
--     bindings = [],
--     operands = Map.singleton k v
--   }
--   res <- op
--   State.put p
--   return res