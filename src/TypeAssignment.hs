module TypeAssignment where

import Debug.Trace (trace)
import qualified AST
import qualified Scope
import qualified Types

data TypedTopLevel = TypedFunction Types.DataType String TypedExpression deriving Show

data TypedExpression = TypedExpression Types.DataType AST.Expression deriving Show

assignTypes :: Scope.Scope -> [AST.TopLevel] -> [TypedTopLevel]
assignTypes scope = map $ assignTopLevelTypes scope

assignTopLevelTypes :: Scope.Scope -> AST.TopLevel -> TypedTopLevel
assignTopLevelTypes scope (AST.TopLevel inner name body) = TypedFunction innerType name $ assignExpressionTypes scope (Just innerType) body
  where innerType = Scope.resolveType scope inner

assignExpressionTypes :: Scope.Scope -> Maybe Types.DataType -> AST.Expression -> TypedExpression
assignExpressionTypes _ (Just outer) (AST.IntLiteral val)
  | Types.isIntType outer = TypedExpression outer $ AST.IntLiteral val
  | otherwise = error "Non-int type for int literal"