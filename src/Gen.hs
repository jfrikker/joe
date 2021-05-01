{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo #-}
module Gen where

import qualified AST
import qualified TypeAssignment
import qualified Types

import Control.Monad (join)
import qualified Data.List as List
import Data.String (fromString)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T
import LLVM.IRBuilder.Monad (IRBuilder, emptyIRBuilder, execIRBuilder)
import LLVM.IRBuilder.Instruction (ret)

gen :: [TypeAssignment.TypedTopLevel] -> LLVM.Module
gen tree = LLVM.defaultModule {
  LLVM.moduleName = "in.j",
  LLVM.moduleDefinitions = join $ map genTopLevel tree
  }

genTopLevel :: TypeAssignment.TypedTopLevel -> [LLVM.Definition]
genTopLevel (TypeAssignment.TypedFunction datatype name body) = [LLVM.GlobalDefinition $ G.functionDefaults {
  G.name = fromString name,
  G.parameters = ([], False),
  G.returnType = Types.toLLVMType datatype,
  G.basicBlocks = bodyCode
  }]
  where bodyCode = execIRBuilder emptyIRBuilder $ exprToLLVM body >>= ret

exprToLLVM :: TypeAssignment.TypedExpression -> IRBuilder LLVM.Operand
exprToLLVM (TypeAssignment.TypedExpression datatype (AST.IntLiteral literal)) = return $ LLVM.ConstantOperand $ Types.constantInt  datatype literal