module Main where

import Lib
import Parser
import Scope

import qualified Data.Text.Lazy.IO as TIO
import LLVM.Pretty (ppllvm)
import Text.Parsec.Text.Lazy (parseFromFile)

main :: IO ()
main = do
  tree <- parseFromFile Parser.mod "in.j"
  print tree