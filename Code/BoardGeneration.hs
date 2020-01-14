module BoardGeneration where

import Parser
import qualified Data.Vector as V

data Tile = Empty | 
            Wall 
            deriving (Eq, Show)

type Board = V.Vector (V.Vector Tile)


printBoard :: Board -> IO ()
printBoard board = if V.null board then putStrLn ""
                                   else do putStrLn (show (V.head board))
                                           printBoard (V.tail board)

checkBoundaries :: (Int, Int) -> Int -> Bool
checkBoundaries (start, end) size = and [start >= 0, start < size, end >= 0, end < size, start <= end]


placeWall :: Board -> Obstacle -> Board
placeWall board ((colS, colE), (rowS, rowE)) = if rowS > rowE then board
                                                  else let updates = [(x, Wall) | x <- [colS .. colE]]
                                                           newRow = (board V.! rowS) V.// updates
                                                           newBoard = board V.// [(rowS, newRow)]
                                                       in  placeWall newBoard ((colS, colE), (rowS + 1, rowE)) 


placeObstacles :: Board -> [Obstacle] -> Either String Board
placeObstacles board [] = Right board
placeObstacles board (((colS, colE), (rowS, rowE)) : os) = let errorMessage = "Incorrect obstacle boundaries: (" ++ show colS ++ "-" ++ show colE ++ ", " ++ show rowS ++ "-" ++ show rowE ++ ") ."
                                                           in  if not (checkBoundaries (colS, colE) (V.length (V.head board)))
                                                                  then Left errorMessage
                                                                  else if not (checkBoundaries (rowS, rowE) (V.length board))
                                                                          then Left errorMessage
                                                                          else placeObstacles (placeWall board ((colS, colE), (rowS, rowE))) os
                                                               

rectangleBoard :: Int -> Int -> Board
rectangleBoard w h = V.replicate h (V.replicate w Empty)


generateBoard :: Map -> Either String Board
generateBoard (Rectangle w h, obstacles)   = if w <= 0 then Left "Board width is not positive."
                                                       else if h <= 0 then Left "Board height is not positive."
                                                                      else placeObstacles (rectangleBoard w h) obstacles
generateBoard (Outline borders, obstacles) = Left "Get outta here."
