module TicTacToe where

import GameAI
import System.IO
import System.Random  

type GameState = ([Int], Int)

startState :: GameState
startState = (replicate 9 0, 1)

shiftPlayer :: Int -> Int
shiftPlayer 1 = 2
shiftPlayer 2 = 1

numToSymbol :: Int -> String
numToSymbol 0 = " "
numToSymbol 1 = "X"
numToSymbol 2 = "O"

mark :: Int -> Int -> GameState -> GameState
mark row col (board, player) = let pos = row * 3 + col
                                   newBoard = (take pos board) ++ [player] ++ (drop (pos + 1) board)
                                   newPlayer = shiftPlayer player
                                in (newBoard, newPlayer)


printRow :: [Int] -> Int -> IO()
printRow board row = let first = numToSymbol (board !! (3 * row))
                         second = numToSymbol (board !! (3 * row + 1))
                         third = numToSymbol (board !! (3 * row + 2))
                         toPrint = first ++ "  |  " ++ second ++ "  |  " ++ third
                      in putStrLn toPrint

printLine :: IO()
printLine = putStrLn "---------------"

printState :: GameState -> IO()
printState (board, _) = do printRow board 0
                           printLine
                           printRow board 1
                           printLine
                           printRow board 2

checkSet :: GameState -> [Int] -> Bool
checkSet (board, player) is = foldr (&&) True (map (\ind -> board!!ind == player) is)

checkRows :: GameState -> Bool
checkRows state = let first = checkSet state [0,1,2]
                      second = checkSet state [3,4,5]
                      third = checkSet state [6,7,8]
                   in or [first, second, third]

checkColumns :: GameState -> Bool
checkColumns state = let first = checkSet state [0,3,6]
                         second = checkSet state [1,4,7]
                         third = checkSet state [2,5,8]
                      in or [first, second, third]
                                
checkDiags :: GameState -> Bool
checkDiags state = let first = checkSet state [0,4,8]
                       second = checkSet state [2,4,6]
                    in or [first, second]

checkVictory :: GameState -> Bool
checkVictory (board, player) = let stateToCheck = (board, shiftPlayer player)
                                in or [checkRows stateToCheck, checkColumns stateToCheck, checkDiags stateToCheck]

playerPlay :: GameState -> IO(GameState)
playerPlay state = do putStr "Ingrese la fila: "
                      row <- getLine
                      putStr "Ingrese la columna: "
                      column <- getLine
                      return (mark (read row) (read column) state)

emptyCenter :: GameState -> Bool
emptyCenter (board, _) = (board !! 4) == 0

countRivalEmptyAux :: [Int] -> GameState -> (Int, Int) -> (Int, Int)
countRivalEmptyAux [] _ (r, e) = (r, e)
countRivalEmptyAux (p:ps) (board, rival) (r, e) = if board !! p == 0 then countRivalEmptyAux ps state (r, e + 1)
                                                                     else if board !! p == rival then countRivalEmptyAux ps state (r + 1, e)
                                                                                                 else countRivalEmptyAux ps state (r, e)

countRivalEmpty :: [Int] -> GameState -> (Int, Int)
countRivalEmpty list state = countRivalEmptyAux list state (0,0)

checkLossRow :: GameState -> Bool
checkLossRow (board, rival) = let first = countRivalEmpty [0,1,2] (board, rival)
                                  second = countRivalEmpty [3,4,5] (board, rival)
                                  third = countRivalEmpty [6,7,8] (board, rival)
                               in or (map (\x -> x == (2,1)) [first, second, third])

checkLossCol :: GameState -> Bool
checkLossCol (board, rival) = let first = countRivalEmpty [0,3,6] (board, rival)
                                  second = countRivalEmpty [1,4,7] (board, rival)
                                  third = countRivalEmpty [2,5,8] (board, rival)
                               in or (map (\x -> x == (2,1)) [first, second, third])

checkLossDiag :: GameState -> Bool
checkLossCol (board, rival) = let first = countRivalEmpty [0,4,8] (board, rival)
                                  second = countRivalEmpty [2,4,6] (board, rival)
                               in or (map (\x -> x == (2,1)) [first, second])

possibleLoss :: GameState -> Bool
possibleLoss (board, player) = let stateToCheck = (board, shiftPlayer player)
                                in or [checkLossRow stateToCheck, checkLossCol stateToCheck, checkLossDiag stateToCheck]

fillRow :: GameState -> GameState
fillRow (board, player) = let rival = shiftPlayer player
                              first = (countRivalEmpty [0,1,2] (board, rival)) == (2,1)
                              second = (countRivalEmpty [3,4,5] (board, rival)) == (2,1)
                           in if first then markRow
                                       else if second then
                                                      else

fillCol :: GameState -> GameState
fillCol (board, player) =

fillDiag :: GameState -> GameState
fillDiag (board, player) =

avoidLoss :: GameState -> GameState
avoidLoss state = If (checkLossRow state)
                     (fillRow state)
                     (If (checkLossCol state)
                         (fillCol state)
                         (fillDiag state))

winningPlay :: GameState -> GameState
winningPlay (board, player) = ...

goForCenter :: GameAI GameState
goForCenter = Action (\state -> mark 1 1 state)

saveYourself :: GameAI GameState
saveYourself = Action (\state -> avoidLoss state)

goForTheWin :: GameAI GameState
goForTheWin = Action (\state -> winningPlaye state)

secondPlay :: GameAI GameState
secondPlay = If possibleLoss
                saveYourself
                goForTheWin

generalPlay :: GameAI GameState
generalPlay = If emptyCenter
                 goForCenter 
                 secondPlay

aiPlay :: GameState -> IO(GameState)
aiPlay state = return (perform (generalPlay state) state)
