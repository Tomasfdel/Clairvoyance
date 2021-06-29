module Main where

import FileParser.Lexer
import FileParser.Parser
import Game.BoardGeneration
import Game.PlayGame
import Game.StatBlockGeneration
import Game.UnitPlacement
import System.Environment

-- ~ Parses the input file and generates all the required data to start the game.
main :: IO ()
main = do
  input <- getArgs
  case input of
    [inFile] -> do
      inContent <- readFile inFile
      let (boardIn, unitIn, aiIn, teamIn) = parse (alexScanTokens inContent)
       in case convertBoardInput boardIn of
            Left errorMsg -> putStrLn ("ERROR: " ++ errorMsg)
            Right (board, offset) -> case convertStatInputs unitIn of
              Left errorMsg -> putStrLn ("ERROR: " ++ errorMsg)
              Right units -> case checkAInames aiIn of
                Left errorMsg -> putStrLn ("ERROR: " ++ errorMsg)
                Right ais -> case placeUnits board offset units ais teamIn of
                  Left errorMsg -> putStrLn ("ERROR: " ++ errorMsg)
                  Right (finalBoard, finalUnits) -> playGame finalBoard finalUnits
    _ -> putStrLn "Error: Se necesita un archivo de entrada."
