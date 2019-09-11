module Nim where

import GameAI
import System.IO

type GameState = [Int]
startState :: GameState
startState = [1,2,3,4,5,6]

nimSum :: [Bool] -> [Bool] -> [Bool]
nimSum [] ys = ys
nimSum xs [] = xs
nimSum (x: xs) (y: ys) = (x /= y) : (nimSum xs ys)
                                 
binTransform :: Int -> [Bool]
binTransform 0 = []
binTransform n = ((mod n 2) == 1) : (binTransform (div n 2))

checkSuccess :: GameState -> Bool
checkSuccess state = all not (foldr nimSum [] (map binTransform state))

reducePile :: Int -> Int -> GameState -> GameState
reducePile index amount xs = (take index xs) ++ [(xs !! index) - amount] ++ (drop (index + 1) xs)

firstPileAux :: GameState -> Int -> Int
firstPileAux (p: ps) ind = if p > 0 then ind
                                    else firstPileAux ps (ind + 1)
firstPile :: GameState -> Int
firstPile ps = firstPileAux ps 0

checkMoves :: GameState -> [GameAI GameState] -> GameAI GameState
checkMoves state [] = Action (\s -> reducePile (firstPile state) 1 s)
checkMoves state (a: as) = if checkSuccess (perform a state) then a
                                                             else checkMoves state as
substract :: Int -> Int -> GameAI GameState
substract index amount = Action (\xs -> reducePile index amount xs)

genSubstract :: GameState -> GameAI GameState
genSubstract nimS = Choose [(substract ind n) | ind <- [0..(length nimS)-1], n <- [1..(nimS!!ind)]] checkMoves

checkVictory :: GameState -> Bool
checkVictory state = all (== 0) state

printState :: GameState -> IO()
printState state = do putStrLn (show state)
                      putStrLn ""
                   
aiPlay :: GameState -> IO(GameState)
aiPlay nimS = return (perform (genSubstract nimS) nimS)

playerPlay :: GameState -> IO(GameState)
playerPlay nimS = do putStr "Ingrese la pila: "
                     pile <- getLine
                     putStr "Ingrese la cantidad: "
                     amount <- getLine
                     return (reducePile (read pile) (read amount) nimS)
