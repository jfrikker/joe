-- {-# LANGUAGE FlexibleContexts #-}

module Joe.JIR where

-- import qualified Control.Monad.Except as Except
-- import qualified Control.Monad.Reader as Reader
-- import qualified Control.Monad.State as State
-- import qualified Data.Map as Map
-- import qualified Joe.AST as AST
-- import qualified Joe.Error as Error
-- import qualified LLVM.Prelude as LLVM

-- type Symbol = LLVM.ShortByteString

-- newtype JIRState = JIRState {
--   symbols :: Map.Map BindingSignature Symbol,
--   defs :: [Definition],
--   sequence :: Int
-- }

-- emptyState :: JIRState
-- emptyState = JIRState {
--   symbols = Map.empty,
--   defs = [],
--   sequence = 0
-- }

-- data BindingSignature = BindingSignature {
--   bindingName :: String
-- } deriving Show

-- data Definition = Definition {
--   defnSymbol :: Symbol,
--   defnClosureType :: Maybe DataType,
--   defnArgType :: Maybe DataType,
--   returnType :: DataType,
--   body :: Expression
-- } deriving Show

-- data ConcreteTypeSignature =
--   Named String |
--   Function ConcreteTypeSignature ConcreteTypeSignature deriving Show

-- data DataType = I32 |
--   I64 |
--   Closure Symbol [DataType] deriving Show

-- data Expression = Expression deriving Show

-- functionTypeHelper :: [DataType] -> DataType -> Int -> (Maybe DataType, Maybe DataType, DataType)
-- functionTypeHelper [] ret _ = (Nothing, Nothing, ret)
-- functionTypeHelper args ret count
--   | future == [] = (Just $ Closure current ret past, Just current, ret)
--   | past == [] = 
--   | otherwise = 
--   where (past, prest) = List.splitAt count args
--         (current : future) = prest
--         ()

-- buildIR :: (Monad m, Except.MonadError Error.CompileError m) => AST.Module -> m [Definition]
-- buildIR mod = do
--   s <- flip Reader.runReaderT mod $ State.execStateT (symbolForFunction main) emptyState
--   return $ bindings s
--   where main = BindingSignature {
--     bindingName = "main"
--   }

-- realizeBinding :: (Monad m, Except.MonadError Error.CompileError m, State.MonadState JIRState m, Reader.MonadReader AST.Module m) => BindingSignature -> m BindingSummary
-- realizeBinding sig = computeIfAbsent $ do
--   (AST.Binding _ astType (AST.PartialDefinition args body)) <- findBinding
--   symb <- generateSymbol $ bindingName sig
--   realizeFunction symb 
--   let 
--   State.modify $ \s -> s { symbols = Map.insert sig symb $ symbolCache s}
--   f symb
--   return symb
--   where computeIfAbsent c = do
--                           existing <- State.gets $ Map.lookup sig . symbolCache
--                           case existing of
--                             Just e -> return e
--                             Nothing -> c
--         findBinding = do
--           astBinding <- Reader.asks $ AST.findBinding $ bindingName sig
--           case astBinding of
--             Nothing -> Except.throwError $ Error.UnknownBinding $ bindingName sig
--             Just b -> return $ b

-- realizeFunction :: (Monad m, Except.MonadError Error.CompileError m, State.MonadState JIRState m, Reader.MonadReader AST.Module m) => BindingSignature -> [AST.PartialDefinition] -> m Symbol