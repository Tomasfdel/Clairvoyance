module AIActions.Escape where

import AIActions.BreadthFirstSearch
import qualified Data.List as L
import qualified Data.Vector as V
import FileParser.Types
import Game.BoardGeneration

-- ~ Returns the maximum tile distance of a given tile map.
getMaxDistance :: Board DistanceMapTile -> Float
getMaxDistance distanceMap = V.foldr (\row restRowsMax -> max restRowsMax (V.foldr (\(_, distance) restMax -> max restMax distance) (-1) row)) (-1) distanceMap

-- ~ Creates a board of floats by using the distance of each valid tile, substracting
-- ~ the maximum distance of the map and multiplying the result by -1.5.
-- ~ The idea is that all valid distance values are reverted but are still non-negative,
-- ~ while assigning a -1 value to all unexplored tiles.
revertDistances :: Board DistanceMapTile -> Float -> Board Float
revertDistances distanceMap maxDistance = V.map (V.map (\(_, distance) -> if distance >= 0 then (- distance + maxDistance) * 1.5 else (-1))) distanceMap

-- ~ Gets the escape value of the escape tile of all valid coordinates in the list
-- ~ plus the given offset.
scanOptions :: Board Tile -> Board Float -> Float -> [Coordinate] -> Coordinate -> [Float]
scanOptions board escapeMap offset coords baseCoord = map (+ offset) (filter (>= 0) (map (\(col, row) -> (escapeMap V.! row) V.! col) (filter (\coord -> isValidMovement board baseCoord coord) coords)))

-- ~ Returns the minimum escape value from the neighbors of a given tile.
scanNeighbors :: Board Tile -> Board Float -> Coordinate -> Maybe Float
scanNeighbors board escapeMap coord =
  let adjacentValues = scanOptions board escapeMap 1 (adjacentStraightCoords coord) coord
      diagonalValues = scanOptions board escapeMap 1.5 (adjacentDiagonalCoords coord) coord
      possibleValues = adjacentValues ++ diagonalValues
   in if null possibleValues
        then Nothing
        else Just (minimum possibleValues)

-- ~ Checks if any adjacent tiles of the given coordinate plus their distance
-- ~ is lower than its current escape value. Returns their new value as
-- ~ well as if it should be updated.
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

-- ~ Scans each tile in the escape map and returns if their value changed and their new value.
scanEscapeTiles :: Board Tile -> Board Float -> [(Coordinate, Float, Bool)]
scanEscapeTiles board escapeMap = map (\coord -> let (newValue, isUpdated) = scanTile board escapeMap coord in (coord, newValue, isUpdated)) (listBoardCoordinates escapeMap)

-- ~ Updates the value of all scanned tiles in the escape map.
updateEscapeMap :: Board Float -> [(Coordinate, Float, Bool)] -> Board Float
updateEscapeMap escapeMap [] = escapeMap
updateEscapeMap escapeMap (((col, row), value, isUpdated) : xs) =
  if not isUpdated
    then updateEscapeMap escapeMap xs
    else
      let newRow = (escapeMap V.! row) V.// [(col, value)]
          newMap = escapeMap V.// [(row, newRow)]
       in updateEscapeMap newMap xs

-- ~ Scans each escape tile, updating their value until no more changes are made.
roundOutCorners :: Board Tile -> Board Float -> Board Float
roundOutCorners board escapeMap =
  let updatedTiles = scanEscapeTiles board escapeMap
   in if not (any (\(_, _, isUpdated) -> isUpdated) updatedTiles)
        then escapeMap
        else roundOutCorners board (updateEscapeMap escapeMap updatedTiles)

-- ~ Given a game board and an associated distance map, creates a board with
-- ~ float values that is later used in the escape algorithm.
buildEscapeMap :: Board Tile -> Board DistanceMapTile -> Board Float
buildEscapeMap board distanceMap = roundOutCorners board (revertDistances distanceMap (getMaxDistance distanceMap))

-- ~ Finds the coordinate to which a unit in the given coordinate should move while escaping.
-- ~ It does it by moving to the adjacent tile with lowest escape value in its sorroundings
-- ~ each step until no more distance can be traveled.
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
