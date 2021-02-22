module Main where

import AIActions
import BoardGeneration
import Control.Monad.State
import qualified Data.Vector as V
import Game
import GameState
import Lexer
import Parser
import StatBlockGeneration
import System.Environment
import System.Random
import UnitPlacement

printGameState :: GameState -> IO ()
printGameState gameState = do
  printBoard (board gameState)
  printUnits (units gameState)
  putStrLn ("Turn Count: " ++ show (turnCount gameState))
  putStrLn ""

turnHandler :: GameState -> (V.Vector Int) -> Int -> IO ()
turnHandler gameState initiative index =
  let (playedTurn, newState) = runState (takeTurn (initiative V.! index)) gameState
      newIndex = mod (index + 1) (V.length initiative)
      newTurn = if newIndex == 0 then turnCount gameState + 1 else turnCount gameState
   in if playedTurn
        then do
          _ <- getLine
          putStrLn ""
          putStrLn ""
          printGameState newState
          putStrLn ("Initiative index: " ++ (show index))
          putStrLn ("Unit index: " ++ (show (initiative V.! index)))
          turnHandler (newState {turnCount = newTurn}) initiative newIndex
        else turnHandler (newState {turnCount = newTurn}) initiative newIndex

playGame :: Board -> (V.Vector Unit) -> IO ()
playGame board units = do
  init <- initiativeRoll units
  putStrLn "Initiative order:"
  putStrLn (show init)
  randomGen <- getStdGen
  let gameState = GameState {board = board, units = units, turnCount = 1, randomGen = randomGen}
   in do
        printGameState gameState
        turnHandler gameState init 0

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
