module TypeAssignment where

import Debug.Trace (trace)
import qualified AST
import qualified Scope
import qualified Types

data TypedTopLevel = TypedFunction Types.DataType String TypedExpression deriving Show

data TypedExpression = 
  TypedAdd Types.DataType TypedExpression TypedExpression |
  TypedIntLiteral Types.DataType Integer deriving Show

assignTypes :: Scope.Scope -> [AST.TopLevel] -> [TypedTopLevel]
assignTypes scope = map $ assignTopLevelTypes scope

assignTopLevelTypes :: Scope.Scope -> AST.TopLevel -> TypedTopLevel
assignTopLevelTypes scope (AST.TopLevel inner name body) = TypedFunction innerType name $ assignExpressionTypes scope (Just innerType) body
  where innerType = Scope.resolveType scope inner

assignExpressionTypes :: Scope.Scope -> Maybe Types.DataType -> AST.Expression -> TypedExpression
assignExpressionTypes _ (Just outer) (AST.IntLiteral val)
  | Types.isIntType outer = TypedIntLiteral outer val
  | otherwise = error "Non-int type for int literal"
assignExpressionTypes scope (Just outer) (AST.Add e1 e2)
  | Types.isIntType outer = TypedAdd outer (assignExpressionTypes scope (Just outer) e1) (assignExpressionTypes scope (Just outer) e2)
  | otherwise = error "Non-int type for int literal"