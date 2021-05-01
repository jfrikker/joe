module Main where

import Gen
import Lib
import Parser
import Scope
import TypeAssignment

import qualified Data.Text.Lazy.IO as TIO
import LLVM.Pretty (ppllvm)
import Text.Parsec.Text.Lazy (parseFromFile)

main :: IO ()
main = do
  tree <- parseFromFile Parser.mod "in.j"
  print tree
  let (Right tree') = tree
  let scope = globalScope
  let tree2 = assignTypes globalScope tree'
  print tree2
  TIO.putStrLn $ ppllvm $ gen tree2