module Main where

import Lexer
import Parser
import BoardGeneration
import UnitPlacement

main = do
  input <- getContents
  let (boardInput, units, teams) = parse (alexScanTokens input)
      board = generateBoard boardInput
   in case board of
           Left errorMsg -> putStrLn ("ERROR: " ++ errorMsg)
           Right (trueBoard, offset) -> putStrLn (show (placeUnits trueBoard units teams offset))
                                             
