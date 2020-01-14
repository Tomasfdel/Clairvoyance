module MapGeneration where

import Util
import qualified Data.Vector as V
import qualified Data.Set as Set
import System.Random  
import System.IO  

-- TO DO: Probablemente reemplazar el tipo por alguna estructura?
type Board = V.Vector (V.Vector Int)

seedBoard :: Board -> Int -> Queue (Int, Int) -> IO (Board, Queue (Int, Int))
seedBoard board 0 seedQueue        = return (board, seedQueue)
seedBoard board numSeeds seedQueue = do seedX <- randomRIO (0, (V.length (board V.! 0))  - 1)
                                        seedY <- randomRIO (0, (V.length board) - 1)
                                        if (board V.! seedY V.! seedX) == 0 
                                           then let newRow = (board V.! seedY) V.// [(seedX, numSeeds)]
                                                    newBoard = board V.// [(seedY, newRow)]
                                                 in seedBoard newBoard (numSeeds - 1) (queuePush seedQueue (seedX, seedY))
                                           else seedBoard board numSeeds seedQueue

setBoard :: Int -> Int -> Int -> IO (Board, Queue (Int, Int))
setBoard height width numSeeds = let emptyBoard = V.replicate height (V.replicate width 0)
                                  in seedBoard emptyBoard numSeeds emptyQueue


-- TO DO: Hacer función updateBoard que me permita cambiar el let newRow...
-- TO DO: Hacer funciones height y width para un board
checkAndSetSeed :: Board -> Queue (Int, Int) -> Int -> Int -> Int -> (Board, Queue (Int, Int))
checkAndSetSeed board queue seedX seedY value = 
                if seedX >= 0 &&
                   seedX < V.length (board V.! 0) &&
                   seedY >= 0 &&
                   seedY < V.length board &&
                   board V.! seedY V.! seedX == 0 
                   then let newRow = (board V.! seedY) V.// [(seedX, value)]
                            newBoard = board V.// [(seedY, newRow)]
                            newQueue = queuePush queue (seedX, seedY)
                         in (newBoard, newQueue)
                   else (board, queue)

expandSeeds :: Board -> Queue (Int, Int) -> Board
expandSeeds board queue = if queueIsEmpty queue 
                             then board
                             else let (seedX, seedY) = queueFront queue
                                      seedValue = board V.! seedY V.! seedX
                                      (board0, queue0) = (board, queuePop queue)
                                      (board1, queue1) = checkAndSetSeed board0 queue0 (seedX - 1) seedY seedValue
                                      (board2, queue2) = checkAndSetSeed board1 queue1 (seedX + 1) seedY seedValue
                                      (board3, queue3) = checkAndSetSeed board2 queue2 seedX (seedY - 1) seedValue
                                      (board4, queue4) = checkAndSetSeed board3 queue3 seedX (seedY + 1) seedValue
                                   in expandSeeds board4 queue4

getCornerNumbers :: Board -> Set.Set Int
getCornerNumbers board = let height = V.length board
                             width = V.length (board V.! 0)
                             topLeft = (board V.! 0) V.! 0
                             topRight = (board V.! 0) V.! (width - 1)
                             botLeft = (board V.! (height - 1)) V.! 0
                             botRight = (board V.! (height - 1)) V.! (width - 1)
                          in Set.fromList [topLeft, topRight, botLeft, botRight]

cleanCell:: V.Vector Int -> Set.Set Int -> Int -> Int
cleanCell row cornerSeeds index = let currentCell = row V.! index
                                   in if (Set.member currentCell cornerSeeds) then 0
                                                                              else 1 

cleanRow :: Board -> Set.Set Int -> Int -> V.Vector Int
cleanRow board cornerSeeds rowNum = let currentRow = board V.! rowNum
                                     in V.generate (V.length currentRow) (cleanCell currentRow cornerSeeds)

cleanBoard :: Board -> Set.Set Int -> Board
cleanBoard board cornerSeeds = V.generate (V.length board) (cleanRow board cornerSeeds)


createBoard :: Int -> Int -> IO Board
createBoard height width = do numSeeds <- randomRIO (2 * (min height width), 2 * (max height width))
                              (board, seedQueue) <- setBoard height width numSeeds
                              let fullBoard = expandSeeds board seedQueue
                                  cornerSeeds = getCornerNumbers fullBoard
                                  shapedBoard = cleanBoard fullBoard cornerSeeds
                              return shapedBoard
                               
                              

-- TO DO: Migrar Board a Util o a un archivo específico
-- TO DO: Armar type Coordinates
printBoard :: Board -> IO ()
printBoard board = if V.null board then putStrLn ""
                                   else do putStrLn (show (V.head board))
                                           printBoard (V.tail board)


main :: IO ()
main = do board <- createBoard 5 10
          printBoard board
