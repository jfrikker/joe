module Main where

import qualified Joe.Parser as Parser

import Text.Parsec.Text.Lazy (parseFromFile)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  tree <- parseFromFile Parser.mod "in.j"
  pPrint tree