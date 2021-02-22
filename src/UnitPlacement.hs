module UnitPlacement where

import BoardGeneration
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import ParserTypes
import StatBlockGeneration

-- TO DO: Cambiar los error msg para que cada función agregue la parte que le corresponde y no sólo el nombre del team, unit o lo que sea.

printUnits :: V.Vector Unit -> IO ()
printUnits units = do
  V.mapM_ (\unit -> putStrLn (show unit)) units
  putStrLn ""

data MobUnit = MobUnit
  { name :: String,
    team :: String,
    identifier :: Int,
    position :: Coordinate,
    statBlock :: StatBlock,
    ai :: Action,
    targets :: [Int]
  }
  deriving (Show)

data Unit = Mob MobUnit
  deriving (Show)

unitIsAlive :: Unit -> Bool
unitIsAlive (Mob unit) = healthPoints (statBlock unit) >= 0

getName :: Unit -> String
getName (Mob unit) = name unit

getTeam :: Unit -> String
getTeam (Mob unit) = team unit

getIdentifier :: Unit -> Int
getIdentifier (Mob unit) = identifier unit

getPosition :: Unit -> Coordinate
getPosition (Mob unit) = position unit

getAI :: Unit -> Action
getAI (Mob unit) = ai unit

getTargets :: Unit -> [Int]
getTargets (Mob unit) = targets unit

getStatBlock :: Unit -> StatBlock
getStatBlock (Mob unit) = statBlock unit

getInitiative :: Unit -> Int
getInitiative (Mob unit) = initiative (statBlock unit)

-- TO DO: Unificar esto con la función de arriba, por favor
duplicateTeamName :: [Team] -> S.Set String -> Maybe String
duplicateTeamName [] _ = Nothing
duplicateTeamName ((name, _) : ts) set =
  if S.member name set
    then Just ("Duplicate team name " ++ name ++ ".")
    else duplicateTeamName ts (S.insert name set)

shiftUnits :: [Team] -> Coordinate -> [Team]
shiftUnits teams offset =
  map
    ( \(team, units) ->
        ( team,
          map
            (\(name, ai, n, positions) -> (name, ai, n, map (\(col, row) -> (col + fst offset, row + snd offset)) positions))
            units
        )
    )
    teams

-- ~ TO DO: Estas funciones se pueden simplificar haciendo un map del campo correspondiente y creando una función del tipo [String] -> (M.Map String a) -> Maybe String
invalidNameInTeam :: [(String, String, Int, [Coordinate])] -> (M.Map String StatBlock) -> Maybe String
invalidNameInTeam [] _ = Nothing
invalidNameInTeam ((name, _, _, _) : us) statMap =
  if M.member name statMap
    then invalidNameInTeam us statMap
    else Just ("Unknown unit name " ++ name ++ " in team ")

invalidAIInTeam :: [(String, String, Int, [Coordinate])] -> (M.Map String Action) -> Maybe String
invalidAIInTeam [] _ = Nothing
invalidAIInTeam ((_, ai, _, _) : us) aiMap =
  if M.member ai aiMap
    then invalidAIInTeam us aiMap
    else Just ("Unknown AI name " ++ ai ++ " in team ")

placeTeamUnits :: Board -> [Coordinate] -> Int -> Either String Board
placeTeamUnits board [] _ = Right board
placeTeamUnits board ((col, row) : cs) index =
  if not (validCoord board (col, row))
    then Left ("Coordinate " ++ (show (col, row)) ++ " out of bounds in unit ")
    else case (board V.! row) V.! col of
      Empty ->
        let newRow = (board V.! row) V.// [(col, Unit index)]
            newBoard = board V.// [(row, newRow)]
         in placeTeamUnits newBoard cs (index + 1)
      _ -> Left ("Invalid unit placement " ++ (show (col, row)) ++ " in unit ")

placeTeam :: Board -> [(String, String, Int, [Coordinate])] -> Int -> Either String Board
placeTeam board [] _ = Right board
placeTeam board ((name, ai, amount, positions) : us) index =
  if amount /= length positions
    then Left ("Mismatched amount and coordinate amount in unit " ++ name ++ " in team ")
    else case placeTeamUnits board positions index of
      Left errorMsg -> Left (errorMsg ++ name ++ " in team ")
      Right newBoard -> placeTeam newBoard us (index + length positions)

buildTeamList :: (M.Map String StatBlock) -> (M.Map String Action) -> String -> [(String, String, Int, [Coordinate])] -> (M.Map String Int) -> [Unit]
buildTeamList _ _ _ [] _ = []
buildTeamList statMap aiMap team ((name, ai, amount, positions) : us) idMap =
  let baseID = if M.member name idMap then idMap M.! name else 1
      (newID, units) =
        L.mapAccumL
          (\idNum pos -> (idNum + 1, Mob MobUnit {name = name, team = team, identifier = idNum, position = pos, statBlock = statMap M.! name, ai = aiMap M.! ai, targets = []}))
          baseID
          positions
   in units ++ buildTeamList statMap aiMap team us (M.insert name newID idMap)

buildTeam :: (M.Map String StatBlock) -> (M.Map String Action) -> String -> [(String, String, Int, [Coordinate])] -> V.Vector Unit
buildTeam statMap aiMap team units = V.fromList (buildTeamList statMap aiMap team units M.empty)

createUnits :: Board -> (M.Map String StatBlock) -> (M.Map String Action) -> [Team] -> V.Vector Unit -> Either String (Board, V.Vector Unit)
createUnits board _ _ [] units = Right (board, units)
createUnits board statMap aiMap ((teamName, teamUnits) : ts) units = case invalidNameInTeam teamUnits statMap of
  Just errorMsg -> Left (errorMsg ++ teamName ++ ".")
  Nothing -> case invalidAIInTeam teamUnits aiMap of
    Just errorMsg -> Left (errorMsg ++ teamName ++ ".")
    Nothing -> case placeTeam board teamUnits (length units) of
      Left errorMsg -> Left (errorMsg ++ teamName ++ ".")
      Right newBoard -> createUnits newBoard statMap aiMap ts (units V.++ buildTeam statMap aiMap teamName teamUnits)

placeUnits :: Board -> Coordinate -> (M.Map String StatBlock) -> (M.Map String Action) -> [Team] -> Either String (Board, V.Vector Unit)
placeUnits board offset units ais teams = case duplicateTeamName teams S.empty of
  Just errorMsg -> Left errorMsg
  Nothing -> createUnits board units ais (shiftUnits teams offset) V.empty
