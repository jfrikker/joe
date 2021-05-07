module Joe.Scope where

import qualified Joe.AST as AST
import qualified Joe.Types as Types

import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Scope = Scope {
  datatypes :: Map.Map String Types.DataType
}

globalScope :: Scope
globalScope = Scope {
  datatypes = Map.fromList [
    ("I32", "I32"),
    ("I64", "I64")
  ]
}

resolveType :: Scope -> AST.TypeSignature -> Types.DataType
resolveType Scope {datatypes = dt} (AST.NamedType name) = fromJust $ Map.lookup name dt