module Main where

import Lexer
import Parser
import BoardGeneration
import StatBlockGeneration
import UnitPlacement
import AIActions


main :: IO()
main = do
  input <- getContents
  let (boardIn, unitIn, aiIn, teamIn) = parse (alexScanTokens input)
   in case convertBoardInput boardIn of
           Left errorMsg -> putStrLn ("ERROR: " ++ errorMsg)
           Right (board, offset) -> case convertStatInputs unitIn of
                                         Left errorMsg -> putStrLn ("ERROR: " ++ errorMsg)
                                         Right units -> case checkAInames aiIn of
                                                             Left errorMsg -> putStrLn ("ERROR: " ++ errorMsg)
                                                             Right ais -> putStrLn (show (placeUnits board offset units ais teamIn))
                                             
