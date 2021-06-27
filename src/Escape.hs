module Escape where

import BoardGeneration
import BreadthFirstSearch
import qualified Data.List as L
import qualified Data.Vector as V
import ParserTypes

getMaxDistance :: Board DistanceMapTile -> Float
getMaxDistance distanceMap = V.foldr (\row restRowsMax -> max restRowsMax (V.foldr (\(_, distance) restMax -> max restMax distance) (-1) row)) (-1) distanceMap

revertDistances :: Board DistanceMapTile -> Float -> Board Float
revertDistances distanceMap maxDistance = V.map (V.map (\(_, distance) -> if distance >= 0 then (- distance + maxDistance) * 1.5 else (-1))) distanceMap

scanOptions :: Board Tile -> Board Float -> Float -> [Coordinate] -> Coordinate -> [Float]
scanOptions board escapeMap offset coords baseCoord = map (+ offset) (filter (>= 0) (map (\(col, row) -> (escapeMap V.! row) V.! col) (filter (\coord -> isValidMovement board baseCoord coord) coords)))

scanNeighbors :: Board Tile -> Board Float -> Coordinate -> Maybe Float
scanNeighbors board escapeMap coord =
  let adjacentValues = scanOptions board escapeMap 1 (adjacentStraightCoords coord) coord
      diagonalValues = scanOptions board escapeMap 1.5 (adjacentDiagonalCoords coord) coord
      possibleValues = adjacentValues ++ diagonalValues
   in if null possibleValues
        then Nothing
        else Just (minimum possibleValues)

scanTile :: Board Tile -> Board Float -> Coordinate -> (Float, Bool)
scanTile board escapeMap (col, row) =
  let currentValue = (escapeMap V.! row) V.! col
   in if currentValue < 0
        then (currentValue, False)
        else case scanNeighbors board escapeMap (col, row) of
          Nothing -> (currentValue, False)
          Just minNeighbourValue ->
            if minNeighbourValue < currentValue
              then (minNeighbourValue, True)
              else (currentValue, False)

scanEscapeTiles :: Board Tile -> Board Float -> [(Coordinate, Float, Bool)]
scanEscapeTiles board escapeMap = map (\coord -> let (newValue, isUpdated) = scanTile board escapeMap coord in (coord, newValue, isUpdated)) (listBoardCoordinates escapeMap)

updateEscapeMap :: Board Float -> [(Coordinate, Float, Bool)] -> Board Float
updateEscapeMap escapeMap [] = escapeMap
updateEscapeMap escapeMap (((col, row), value, isUpdated) : xs) =
  if not isUpdated
    then updateEscapeMap escapeMap xs
    else
      let newRow = (escapeMap V.! row) V.// [(col, value)]
          newMap = escapeMap V.// [(row, newRow)]
       in updateEscapeMap newMap xs

roundOutCorners :: Board Tile -> Board Float -> Board Float
roundOutCorners board escapeMap =
  let updatedTiles = scanEscapeTiles board escapeMap
   in if not (any (\(_, _, isUpdated) -> isUpdated) updatedTiles)
        then escapeMap
        else roundOutCorners board (updateEscapeMap escapeMap updatedTiles)

buildEscapeMap :: Board Tile -> Board DistanceMapTile -> Board Float
buildEscapeMap board distanceMap = roundOutCorners board (revertDistances distanceMap (getMaxDistance distanceMap))

findEscapePosition :: Board Tile -> Board Float -> Coordinate -> Float -> Coordinate
findEscapePosition board escapeMap (startCol, startRow) distanceLeft =
  let straightAdjacent =
        map
          (\(col, row) -> ((col, row), (escapeMap V.! row) V.! col, 1))
          (filter (isValidMovement board (startCol, startRow)) (adjacentStraightCoords (startCol, startRow)))
      diagonalAdjacent =
        map
          (\(col, row) -> ((col, row), (escapeMap V.! row) V.! col, 1.5))
          (filter (isValidMovement board (startCol, startRow)) (adjacentDiagonalCoords (startCol, startRow)))
      sortedAdjacents = L.sortOn (\(_, escapeValue, _) -> escapeValue) (straightAdjacent ++ diagonalAdjacent)
   in case L.uncons sortedAdjacents of
        Nothing -> (startCol, startRow)
        Just ((newPosition, escapeValue, distance), _) ->
          if escapeValue < (escapeMap V.! startRow) V.! startCol
            && distance <= fromIntegral (ceiling distanceLeft)
            then findEscapePosition board escapeMap newPosition (distanceLeft - distance)
            else (startCol, startRow)
