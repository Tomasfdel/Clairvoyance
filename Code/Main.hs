module Main where

import Lexer
import Parser
import BoardGeneration
import StatBlockGeneration
import UnitPlacement
import AIActions
import GameState
import Game
import qualified Data.Vector as V
import System.Random
import System.Environment

printGameState :: GameState -> IO ()
printGameState gameState = do printBoard (board gameState)
                              printUnits (units gameState)
                              putStrLn (show (turnCount gameState))
                              putStrLn ""


-- TO DO: Esto también debería ser una stateful computation?
turnHandler :: GameState -> (V.Vector Int) -> Int -> IO()
turnHandler gameState initiative index = do putStrLn ""
                                            putStrLn ""
                                            printGameState gameState
                                            putStrLn ("Initiative index: " ++ (show index))
                                            putStrLn ("Unit index: " ++ (show (initiative V.! index)))
                                            _ <- getLine
                                            newState <- takeTurn gameState (initiative V.! index)
                                            let newIndex = mod (index + 1) (V.length initiative)
                                                newTurn = if newIndex == 0 then turnCount gameState + 1 else turnCount gameState
                                             in turnHandler (newState {turnCount = newTurn}) initiative newIndex


playGame :: Board -> (V.Vector Unit) -> IO()
playGame board units = do init <- initiativeRoll units
                          putStrLn "Initiative order:"
                          putStrLn (show init)
                          randomGen <- getStdGen
                          turnHandler (GameState {board = board, units = units, turnCount = 1, randomGen = randomGen}) init 0
                          
                          
main :: IO()
main = do
  input <- getArgs
  case input of
       [inFile] -> do inContent <- readFile inFile
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
