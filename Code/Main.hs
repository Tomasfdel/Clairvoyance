module Main where

import Lexer
import Parser

main = do
  input <- getContents
  print (alexScanTokens input)
  print (parse (alexScanTokens input))
