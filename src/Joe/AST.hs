module Joe.AST where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Module = [TopLevel]

data TopLevel = GlobalBinding Binding deriving Show

data Binding = Binding String TypeSignature PartialDefinition deriving Show

data Match = Unbound String deriving Show

data PartialDefinition = PartialDefinition [Match] Expression deriving Show

data TypeSignature = 
  NamedType String |
  FunctionType TypeSignature TypeSignature deriving Show

data Expression =
  Add Expression Expression |
  Call Expression Expression |
  I32Literal Integer |
  I64Literal Integer |
  Reference String deriving Show

typeSignatureArguments :: TypeSignature -> Int
typeSignatureArguments (FunctionType _ res) = 1 + typeSignatureArguments res
typeSignatureArguments _ = 0

partialDefinitionArguments :: PartialDefinition -> Int
partialDefinitionArguments (PartialDefinition args _) = length args

topLevelBindings :: Module -> [Binding]
topLevelBindings = Maybe.mapMaybe isBinding
  where isBinding (GlobalBinding b) = Just b

findBinding :: String -> Module -> Maybe Binding
findBinding name = List.find match . topLevelBindings
  where match (Binding bName _ _) = bName == name