module BoardGeneration where

import qualified Data.Vector as V
import ParserTypes

data Tile
  = Empty
  | Wall
  | Border
  | Unit Int
  deriving (Eq, Show)

type Board a = V.Vector (V.Vector a)

listProduct :: [a] -> [b] -> [(a, b)]
listProduct xs ys = [(x, y) | x <- xs, y <- ys]

-- TO DO: Consider extracting methods specific of the board to another file and leave this for strictly board generation.
-- TO DO: This is a partial function, but I want it to be partial.
unitIndex :: Tile -> Int
unitIndex (Unit index) = index

showBoard :: Show a => Board a -> String
showBoard board = foldr (\row rest -> (show row) ++ "\n" ++ rest) "" board

-- ~ Rough print of the board used for debugging purposes.
printBoard :: Show a => Board a -> IO ()
printBoard board = putStr (showBoard board)

listBoardCoordinates :: Board a -> [Coordinate]
listBoardCoordinates board =
  let height = V.length board
      width = V.length (V.head board)
   in listProduct [0 .. width - 1] [0 .. height - 1]

-- ~ Validates that the start and end of a wall section fit in the board dimensions.
checkBoundaries :: (Int, Int) -> Int -> Bool
checkBoundaries (start, end) size = and [start >= 0, start < size, end >= 0, end < size, start <= end]

-- ~ Places walls in the wall section described, row by row.
placeWall :: Board Tile -> Obstacle -> Board Tile
placeWall board ((colS, colE), (rowS, rowE)) =
  if rowS > rowE
    then board
    else
      let updates = [(x, Wall) | x <- [colS .. colE]]
          newRow = (board V.! rowS) V.// updates
          newBoard = board V.// [(rowS, newRow)]
       in placeWall newBoard ((colS, colE), (rowS + 1, rowE))

-- ~ Fills the board with walls on the tiles with coordinates referenced in the obstacle list.
placeObstacles :: Board Tile -> [Obstacle] -> Either String (Board Tile)
placeObstacles board [] = Right board
placeObstacles board (((colS, colE), (rowS, rowE)) : os) =
  let errorMessage = "Incorrect obstacle boundaries: (" ++ show colS ++ "-" ++ show colE ++ ", " ++ show rowS ++ "-" ++ show rowE ++ ") ."
   in if not (checkBoundaries (colS, colE) (V.length (V.head board)))
        then Left errorMessage
        else
          if not (checkBoundaries (rowS, rowE) (V.length board))
            then Left errorMessage
            else placeObstacles (placeWall board ((colS, colE), (rowS, rowE))) os

-- ~ Generates an empty rectangular board of the given dimensions
rectangleBoard :: Int -> Int -> Board Tile
rectangleBoard w h = V.replicate h (V.replicate w Empty)

-- ~ Checks all values of the map border are greater than zero.
checkBorderValues :: [(Int, Direction)] -> Bool
checkBorderValues xs = all (\(i, _) -> i > 0) xs

-- ~ Checks that the map outline does start and end on the same tile.
-- ~ If it does, returns the maximum position values in each direction.
walkBorders :: [(Int, Direction)] -> Coordinate -> V.Vector Int -> Either String (V.Vector Int)
walkBorders [] (0, 0) maxValues = Right maxValues
walkBorders [] (h, v) _ = Left ("Border path does not finish at the start point. Offset: (" ++ (show h) ++ ", " ++ (show v) ++ ") .")
walkBorders ((n, DirLeft) : bs) (col, row) maxValues = walkBorders bs (col - n, row) (maxValues V.// [(0, min (col - n) (maxValues V.! 0))])
walkBorders ((n, DirRight) : bs) (col, row) maxValues = walkBorders bs (col + n, row) (maxValues V.// [(1, max (col + n) (maxValues V.! 1))])
walkBorders ((n, DirUp) : bs) (col, row) maxValues = walkBorders bs (col, row - n) (maxValues V.// [(2, min (row - n) (maxValues V.! 2))])
walkBorders ((n, DirDown) : bs) (col, row) maxValues = walkBorders bs (col, row + n) (maxValues V.// [(3, max (row + n) (maxValues V.! 3))])

-- ~ Walks the outline of the map while placing walls in the described tiles.
placeBorders :: Board Tile -> [(Int, Direction)] -> Coordinate -> Board Tile
placeBorders board [] _ = board
placeBorders board ((n, DirLeft) : bs) (col, row) =
  let updates = [(x, Border) | x <- [col - n .. col]]
      newRow = (board V.! row) V.// updates
      newBoard = board V.// [(row, newRow)]
   in placeBorders newBoard bs (col - n, row)
placeBorders board ((n, DirRight) : bs) (col, row) =
  let updates = [(x, Border) | x <- [col .. col + n]]
      newRow = (board V.! row) V.// updates
      newBoard = board V.// [(row, newRow)]
   in placeBorders newBoard bs (col + n, row)
placeBorders board ((n, DirUp) : bs) (col, row) =
  let newRows = [(y, (board V.! y) V.// [(col, Border)]) | y <- [row - n .. row]]
      newBoard = board V.// newRows
   in placeBorders newBoard bs (col, row - n)
placeBorders board ((n, DirDown) : bs) (col, row) =
  let newRows = [(y, (board V.! y) V.// [(col, Border)]) | y <- [row .. row + n]]
      newBoard = board V.// newRows
   in placeBorders newBoard bs (col, row + n)

-- ~ Checks the given coordinate is a valid position on the board.
validCoord :: Board a -> Coordinate -> Bool
validCoord board (col, row) = and [col >= 0, col < (V.length (V.head board)), row >= 0, row < (V.length board)]

adjacentStraightCoords :: Coordinate -> [Coordinate]
adjacentStraightCoords (col, row) = [(col, row -1), (col, row + 1), (col -1, row), (col + 1, row)]

adjacentDiagonalCoords :: Coordinate -> [Coordinate]
adjacentDiagonalCoords (col, row) = [(col - 1, row - 1), (col - 1, row + 1), (col + 1, row - 1), (col + 1, row + 1)]

-- ~ Starting from a position, replaces the old tile for the new one and calls
-- ~ the function on adjacent cells.
floodFill :: Board Tile -> [Coordinate] -> Tile -> Tile -> Board Tile
floodFill board [] _ _ = board
floodFill board ((col, row) : cs) old new =
  if validCoord board (col, row) && ((board V.! row) V.! col) == old
    then
      let newRow = (board V.! row) V.// [(col, new)]
          newBoard = board V.// [(row, newRow)]
          newCoords = adjacentStraightCoords (col, row)
       in floodFill newBoard (cs ++ newCoords) old new
    else floodFill board cs old new

-- ~ Replaces all the tiles of the old type for the new type in the board.
replaceTiles :: Board Tile -> Tile -> Tile -> Board Tile
replaceTiles board old new = V.map (\row -> V.map (\tile -> if tile == old then new else tile) row) board

-- ~ Adds an offset to te coordinates of all the listed obstacles.
offsetObstacles :: [Obstacle] -> Coordinate -> [Obstacle]
offsetObstacles obstacles (col, row) = map (\(h, v) -> ((fst h + col, snd h + col), (fst v + row, snd v + row))) obstacles

-- ~ Generates a game board based on the given description or returns an error message.
convertBoardInput :: Map -> Either String (Board Tile, Coordinate)
convertBoardInput (Rectangle w h, obstacles) =
  if w <= 0
    then Left "Board width is not positive."
    else
      if h <= 0
        then Left "Board height is not positive."
        else case placeObstacles (rectangleBoard w h) obstacles of
          Left errorMsg -> Left errorMsg
          Right board -> Right (board, (0, 0))
convertBoardInput (Outline borders, obstacles) =
  if not (checkBorderValues borders)
    then Left "Invalid outline value."
    else case walkBorders borders (0, 0) (V.replicate 4 0) of
      Left errorMessage -> Left errorMessage
      Right maxValues ->
        let minH = maxValues V.! 0
            maxH = maxValues V.! 1
            minV = maxValues V.! 2
            maxV = maxValues V.! 3
            baseBoard = rectangleBoard (maxH - minH + 3) (maxV - minV + 3)
            originOffset = (- minH + 1, - minV + 1)
            borderBoard = placeBorders baseBoard borders originOffset
            wallBoard = floodFill borderBoard [(0, 0)] Empty Wall
            finalBoard = replaceTiles wallBoard Border Empty
            newObstacles = offsetObstacles obstacles originOffset
         in case placeObstacles finalBoard newObstacles of
              Left errorMsg -> Left errorMsg
              Right board -> Right (board, originOffset)
