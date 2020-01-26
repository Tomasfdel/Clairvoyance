module UnitPlacement where

import ParserTypes
import BoardGeneration
import StatBlockGeneration
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

-- TO DO: Cambiar los error msg para que cada función agregue la parte que le corresponde y no sólo el nombre del team, unit o lo que sea.

data MobUnit = MobUnit {
                  name :: String,
                  team :: String,
                  position :: Coordinate,
                  statBlock :: StatBlock,
                  ai :: Action }
               deriving Show

data Unit = Mob MobUnit
            deriving Show

-- TO DO: Unificar esto con la función de arriba, por favor
duplicateTeamName :: [Team] -> S.Set String -> Maybe String
duplicateTeamName [] _ = Nothing
duplicateTeamName ((name, _) : ts) set = if S.member name set then Just ("Duplicate team name " ++ name ++ ".")
                                                              else duplicateTeamName ts (S.insert name set)

shiftUnits :: [Team] -> Coordinate -> [Team]
shiftUnits teams offset = map 
                          (\(team, units) -> (team, map 
                                                    (\(name, ai, n, positions) -> (name, ai, n, map (\(col, row) -> (col + fst offset, row + snd offset)) positions)) 
                                                    units)) 
                          teams

invalidNameInTeam :: [(String, String, Int, [Coordinate])] -> (M.Map String StatBlock) -> Maybe String
invalidNameInTeam [] _ = Nothing
invalidNameInTeam ((name, _, _, _) : us) statMap = if M.member name statMap then invalidNameInTeam us statMap
                                                                         else Just ("Unknown unit name " ++ name ++ " in team ")

placeTeamUnits :: Board -> [Coordinate] -> Int -> Either String Board
placeTeamUnits board [] _ = Right board
placeTeamUnits board ((col, row) : cs) index = if not (validCoord board (col, row))
                                                  then Left ("Coordinate " ++ (show (col, row)) ++ " out of bounds in unit ")
                                                  else case (board V.! row) V.! col of
                                                             Empty -> let newRow = (board V.! row) V.// [(col, Unit index)]
                                                                          newBoard = board V.// [(row, newRow)]
                                                                       in placeTeamUnits newBoard cs (index + 1)
                                                             _ -> Left ("Invalid unit placement " ++ (show (col, row)) ++ " in unit ")


placeTeam :: Board -> [(String, String, Int, [Coordinate])] -> Int -> Either String Board
placeTeam board [] _ = Right board
placeTeam board ((name, ai, amount, positions) : us) index = if amount /= length positions 
                                                               then Left ("Mismatched amount and coordinate amount in unit " ++ name ++ " in team ")
                                                               else case placeTeamUnits board positions index of
                                                                         Left errorMsg -> Left (errorMsg ++ name ++ " in team ")
                                                                         Right newBoard -> placeTeam newBoard us (index + length positions)


buildTeamUnits :: (M.Map String StatBlock) -> (M.Map String Action) -> String -> (String, String, Int, [Coordinate]) -> [Unit]
buildTeamUnits statMap aiMap team (name, ai, amount, positions) = map (\pos -> Mob MobUnit {name = name, team = team, position = pos, statBlock = statMap M.! name, ai = aiMap M.! ai}) positions

buildTeam :: (M.Map String StatBlock) -> (M.Map String Action) -> String -> [(String, String, Int, [Coordinate])] -> V.Vector Unit
buildTeam statMap aiMap team units = V.fromList (concat (map (buildTeamUnits statMap aiMap team) units))


createUnits :: Board -> (M.Map String StatBlock) -> (M.Map String Action) -> [Team] -> V.Vector Unit -> Either String (Board, V.Vector Unit)
createUnits board _ _ [] units = Right (board, units)
createUnits board statMap aiMap ((teamName, positions) : ts) units = case invalidNameInTeam positions statMap of
                                                                          Just errorMsg -> Left (errorMsg ++ teamName ++ ".")
                                                                          Nothing -> case placeTeam board positions (length units) of
                                                                                          Left errorMsg -> Left (errorMsg ++ teamName ++ ".")
                                                                                          Right newBoard -> createUnits newBoard statMap aiMap ts (units V.++ buildTeam statMap aiMap teamName positions)

placeUnits :: Board -> Coordinate -> (M.Map String StatBlock) -> (M.Map String Action) -> [Team] -> Either String (Board, V.Vector Unit)
placeUnits board offset units ais teams = case duplicateTeamName teams S.empty of
                                               Just errorMsg -> Left errorMsg
                                               Nothing -> createUnits board units ais (shiftUnits teams offset) V.empty

