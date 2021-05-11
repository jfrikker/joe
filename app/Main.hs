module Main where

import qualified Data.Text.Lazy.IO as TIO
import Joe (compile)
import qualified Joe.Parser as Parser
import LLVM.Pretty (ppllvm)
import Text.Parsec.Text.Lazy (parseFromFile)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  tree <- parseFromFile Parser.mod "in.j"
  pPrint tree
  let (Right tree') = tree;
  let (Right mod) = compile tree'
  TIO.putStrLn $ ppllvm mod