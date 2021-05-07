module Main where

import qualified Joe.Parser as Parser

import Text.Parsec.Text.Lazy (parseFromFile)

main :: IO ()
main = do
  tree <- parseFromFile Parser.mod "in.j"
  print tree