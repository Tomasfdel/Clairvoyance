module LineOfSight where

import BoardGeneration
import BreadthFirstSearch
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V
import ParserTypes

listProduct :: [a] -> [b] -> [(a, b)]
listProduct xs ys = [(x, y) | x <- xs, y <- ys]

listBoardCoordinates :: Board a -> [Coordinate]
listBoardCoordinates board =
  let height = V.length board
      width = V.length (V.head board)
   in listProduct [0 .. width - 1] [0 .. height - 1]

cornerCoordinates :: Coordinate -> [Coordinate]
cornerCoordinates (col, row) = [(col, row), (col, row + 1), (col + 1, row), (col + 1, row + 1)]

anyWallsInList :: Board Tile -> [Coordinate] -> Bool
anyWallsInList board coordList = any (\(col, row) -> (board V.! row) V.! col == Wall) coordList

allWallsInList :: Board Tile -> [Coordinate] -> Bool
allWallsInList board coordList = all (\(col, row) -> (board V.! row) V.! col == Wall) coordList

-- ~ TO DO: Try to generalize this function and all below.
checkWallsCorner :: Board Tile -> Coordinate -> Bool
checkWallsCorner board (col, row) = anyWallsInList board [(col - 1, row - 1), (col - 1, row), (col, row - 1), (col, row)]

checkWallsHoriz :: Board Tile -> Coordinate -> Bool
checkWallsHoriz board (col, row) = anyWallsInList board [(col - 1, row), (col, row)]

checkWallsVert :: Board Tile -> Coordinate -> Bool
checkWallsVert board (col, row) = anyWallsInList board [(col, row - 1), (col, row)]

lineIntersectsHoriz :: Board Tile -> Coordinate -> Int -> Int -> [Int] -> Bool
lineIntersectsHoriz _ _ _ _ [] = False
lineIntersectsHoriz board (baseCol, baseRow) colDiff rowDiff (column : cs) =
  if mod ((column - baseCol) * rowDiff) colDiff == 0
    then checkWallsCorner board (column, baseRow + div ((column - baseCol) * rowDiff) colDiff) || lineIntersectsHoriz board (baseCol, baseRow) colDiff rowDiff cs
    else checkWallsHoriz board (column, baseRow + div ((column - baseCol) * rowDiff) colDiff) || lineIntersectsHoriz board (baseCol, baseRow) colDiff rowDiff cs

lineIntersectsVert :: Board Tile -> Coordinate -> Int -> Int -> [Int] -> Bool
lineIntersectsVert _ _ _ _ [] = False
lineIntersectsVert board (baseCol, baseRow) colDiff rowDiff (row : rs) =
  if mod ((row - baseRow) * colDiff) rowDiff == 0
    then checkWallsCorner board (baseCol + div ((row - baseRow) * colDiff) rowDiff, row) || lineIntersectsVert board (baseCol, baseRow) colDiff rowDiff rs
    else checkWallsVert board (baseCol + div ((row - baseRow) * colDiff) rowDiff, row) || lineIntersectsVert board (baseCol, baseRow) colDiff rowDiff rs

isLineBlockedHoriz :: Board Tile -> Coordinate -> Coordinate -> Bool
isLineBlockedHoriz board coord1 coord2 =
  let [(colStart, rowStart), (colEnd, rowEnd)] = L.sortOn fst [coord1, coord2]
      colDiff = colEnd - colStart
      rowDiff = rowEnd - rowStart
   in lineIntersectsHoriz board (colStart, rowStart) colDiff rowDiff [colStart + 1 .. colEnd - 1]

isLineBlockedVert :: Board Tile -> Coordinate -> Coordinate -> Bool
isLineBlockedVert board coord1 coord2 =
  let [(colStart, rowStart), (colEnd, rowEnd)] = L.sortOn snd [coord1, coord2]
      colDiff = colEnd - colStart
      rowDiff = rowEnd - rowStart
   in lineIntersectsVert board (colStart, rowStart) colDiff rowDiff [rowStart + 1 .. rowEnd - 1]

isLineBlocked :: Board Tile -> Coordinate -> Coordinate -> Bool
isLineBlocked board coord1 coord2 = isLineBlockedHoriz board coord1 coord2 || isLineBlockedVert board coord1 coord2

isWallDiagonal :: Board Tile -> Coordinate -> Coordinate -> Bool
isWallDiagonal board (col1, row1) (col2, row2) =
  abs (col1 - col2) == 1
    && abs (row1 - row2) == 1
    && (board V.! (min row1 row2)) V.! (min col1 col2) == Wall

checkCornersLineOfSight :: Board Tile -> (Coordinate, Coordinate) -> Bool
checkCornersLineOfSight board ((col1, row1), (col2, row2)) =
  col1 /= col2
    && row1 /= row2
    && not (isWallDiagonal board (col1, row1) (col2, row2))
    && not (isLineBlocked board (col1, row1) (col2, row2))

checkLineOfSightHoriz :: Board Tile -> Coordinate -> Coordinate -> Bool
checkLineOfSightHoriz board (col1, row1) (col2, row2) = row1 == row2 && not (anyWallsInList board (map (\col -> (col, row1)) [(min col1 col2 + 1) .. (max col1 col2 - 1)]))

checkLineOfSightVert :: Board Tile -> Coordinate -> Coordinate -> Bool
checkLineOfSightVert board (col1, row1) (col2, row2) = col1 == col2 && not (anyWallsInList board (map (\row -> (col1, row)) [(min row1 row2 + 1) .. (max row1 row2 - 1)]))

peeksThroughDiagonalWall :: Board Tile -> Coordinate -> Coordinate -> Bool
peeksThroughDiagonalWall board (col1, row1) (col2, row2) =
  allWallsInList board [(col1 + signum (col2 - col1), row1), (col1, row1 + signum (row2 - row1))]
    || allWallsInList board [(col2 + signum (col1 - col2), row2), (col2, row2 + signum (row1 - row2))]

checkCoordLineOfSight :: Board Tile -> Coordinate -> Coordinate -> Bool
checkCoordLineOfSight board coord1 coord2 =
  checkLineOfSightHoriz board coord1 coord2
    || checkLineOfSightVert board coord1 coord2
    || ( not (peeksThroughDiagonalWall board coord1 coord2)
           && any (checkCornersLineOfSight board) (listProduct (cornerCoordinates coord1) (cornerCoordinates coord2))
       )

getValidAttackPositions :: Board Tile -> Coordinate -> Int -> S.Set Coordinate
getValidAttackPositions board startCoord maxRange =
  let distanceMap = buildFloatingDistanceMap board startCoord
      boardCoordinates = listBoardCoordinates board
      coordinatesInRange =
        filter
          ( \(col, row) ->
              let tile = (distanceMap V.! row) V.! col
               in (isTileVisited tile) && (tileDistance tile) <= maxRange
          )
          boardCoordinates
      coordinatesInSight = filter (checkCoordLineOfSight board startCoord) coordinatesInRange
   in S.fromList coordinatesInSight
