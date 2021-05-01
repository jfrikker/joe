{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo #-}
module Gen where

import qualified AST

import Control.Monad (join)
import qualified Data.List as List
import Data.String (fromString)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T
import LLVM.IRBuilder.Monad (IRBuilder, emptyIRBuilder, execIRBuilder)
import LLVM.IRBuilder.Instruction (ret)

gen :: [AST.TopLevel] -> LLVM.Module
gen tree = LLVM.defaultModule {
  LLVM.moduleName = "in.j",
  LLVM.moduleDefinitions = join $ map genTopLevel tree
  }

genTopLevel :: AST.TopLevel -> [LLVM.Definition]
genTopLevel (AST.TopLevel _ name body) = [LLVM.GlobalDefinition $ G.functionDefaults {
  G.name = fromString name,
  G.parameters = ([], False),
  G.returnType = T.i32,
  G.basicBlocks = bodyCode
  }]
  where bodyCode = execIRBuilder emptyIRBuilder $ exprToLLVM body >>= ret

exprToLLVM :: AST.Expression -> IRBuilder LLVM.Operand
exprToLLVM (AST.IntLiteral literal) = return $ LLVM.ConstantOperand $ C.Int 32 $ fromIntegral literal