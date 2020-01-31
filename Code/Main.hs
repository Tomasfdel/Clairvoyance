module Main where

import Lexer
import Parser
import BoardGeneration
import StatBlockGeneration
import UnitPlacement
import AIActions
import Game
import qualified Data.Vector as V

turnHandler :: Board -> (V.Vector Unit) -> Int -> (V.Vector Int) -> Int -> IO()
turnHandler board units turnCount initiative index = do (newBoard, newUnits) <- takeTurn board units (initiative V.! index)
                                                        turnHandler newBoard newUnits (turnCount + (div (index + 1) (V.length initiative))) initiative (mod (index + 1) (V.length initiative))

playGame :: Board -> (V.Vector Unit) -> IO()
playGame board units = do init <- initiativeRoll units
                          turnHandler board units 1 init 0
                          
                          
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
                                                             Right ais -> case placeUnits board offset units ais teamIn of
                                                                               Left errorMsg -> putStrLn ("ERROR: " ++ errorMsg)
                                                                               Right (finalBoard, finalUnits) -> playGame finalBoard finalUnits                          
