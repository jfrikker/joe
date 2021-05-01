module Main where

import Gen
import Lib
import Parser

import qualified Data.Text.Lazy.IO as TIO
import LLVM.Pretty (ppllvm)
import Text.Parsec.Text.Lazy (parseFromFile)

main :: IO ()
main = do
  tree <- parseFromFile Parser.mod "in.j"
  print tree
  let (Right tree') = tree
  TIO.putStrLn $ ppllvm $ gen $ tree'