module UnitPlacement where

import Parser
import BoardGeneration
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

-- TO DO: Cambiar los error msg para que cada función agregue la parte que le corresponde y no sólo el nombre del team, unit o lo que sea.


data MobUnit = MobUnit {
                  name :: String,
                  team :: String,
                  position :: Coordinate,
                  statBlock :: StatBlock }
               deriving Show

data Unit = Mob MobUnit
            deriving Show

data UnitSaves = UnitSaves {
                     fortitude :: Int,
                     reflex :: Int,
                     will :: Int } 
                 deriving Show
                 
unitSavesSize :: Int
unitSavesSize = 3

data StatBlock = StatBlock {
                     healthPoints :: Int, 
                     initiative :: Int,
                     speed :: Int,
                     armorClass :: Int,
                     attack :: AttackDesc,
                     fullAttack :: [AttackDesc],
                     saves :: UnitSaves } 
                 deriving Show
                 
statBlockSize :: Int
statBlockSize = 7


data StatTypes = IntType Int              |
                 AttackType AttackDesc    |
                 AttacksType [AttackDesc] |
                 SavesType UnitSaves
                 deriving Show


statBlockFromMap :: M.Map String StatTypes -> StatBlock
statBlockFromMap map = let (IntType healthPoints)   = map M.! "HP"
                           (IntType initiative)     = map M.! "Initiative"
                           (IntType speed)          = map M.! "Speed"
                           (IntType armorClass)     = map M.! "AC"
                           (AttackType attack)      = map M.! "Attack"
                           (AttacksType fullAttack) = map M.! "FullAttack"
                           (SavesType saves)        = map M.! "Saves"
                        in StatBlock { healthPoints = healthPoints,
                                       initiative = initiative,
                                       speed = speed,
                                       armorClass = armorClass,
                                       attack = attack,
                                       fullAttack = fullAttack,
                                       saves = saves }

validAttack :: AttackDesc -> Bool
validAttack (_, _, roll) = (dieAmount roll) >= 0 && (dieValue roll) > 0

-- TO DO: Limpiar esta función, por el amor de Dios
buildUnitSaves :: [SaveInput] -> M.Map String Int -> Either String UnitSaves
buildUnitSaves [] map = if M.size map == unitSavesSize then Right (UnitSaves { fortitude = (map M.! "Fortitude"),
                                                                               reflex = (map M.! "Reflex"),
                                                                               will = (map M.! "Will") } )
                                                       else Left "Missing saves in unit "
buildUnitSaves ((Fortitude n) : ss) map = if M.member "Fortitude" map then Left "Duplicate Fortitude field in saves of unit "
                                                                      else buildUnitSaves ss (M.insert "Fortitude" n map)
buildUnitSaves ((Reflex n) : ss) map = if M.member "Reflex" map then Left "Duplicate Reflex field in saves of unit "
                                                                else buildUnitSaves ss (M.insert "Reflex" n map)
buildUnitSaves ((Will n) : ss) map = if M.member "Will" map then Left "Duplicate Will field in saves of unit "
                                                            else buildUnitSaves ss (M.insert "Will" n map)

-- TO DO: Limpiar esta función, por el amor de Dios
buildStatBlock :: [StatInput] -> M.Map String StatTypes -> Either String StatBlock
buildStatBlock [] map = if M.size map == statBlockSize then Right (statBlockFromMap map)
                                                       else Left "Missing statistics in unit "
buildStatBlock ((HP n) : ss) map = if M.member "HP" map then Left "Duplicate HP field in unit "
                                                        else if n <= 0 then Left "Negative or null HP points in unit "
                                                                       else buildStatBlock ss (M.insert "HP" (IntType n) map)
buildStatBlock ((Initiative n) : ss) map = if M.member "Initiative" map then Left "Duplicate Initiative field in unit "
                                                                        else buildStatBlock ss (M.insert "Initiative" (IntType n) map)
buildStatBlock ((Speed n) : ss) map = if M.member "Speed" map then Left "Duplicate Speed field in unit "
                                                              else if n < 0 then Left "Negative Speed in unit "
                                                                            else buildStatBlock ss (M.insert "Speed" (IntType n) map)
buildStatBlock ((AC n) : ss) map = if M.member "AC" map then Left "Duplicate AC field in unit "
                                                        else buildStatBlock ss (M.insert "AC" (IntType n) map)
buildStatBlock ((Attack n) : ss) map = if M.member "Attack" map then Left "Duplicate Attack field in unit "
                                                                else if not (validAttack n) then Left "Invalid Attack in unit "
                                                                                            else buildStatBlock ss (M.insert "Attack" (AttackType n) map)
buildStatBlock ((FullAttack ns) : ss) map = if M.member "FullAttack" map then Left "Duplicate Full Attack field in unit "
                                                                          else if not (all validAttack ns) then Left "Invalid Full Attack in unit "
                                                                                                                else buildStatBlock ss (M.insert "FullAttack" (AttacksType ns) map)
buildStatBlock ((Saves ns) : ss) map = if M.member "Saves" map then Left "Duplicate Saves field in unit "
                                                               else case buildUnitSaves ns M.empty of
                                                                         Left errorMsg -> Left errorMsg
                                                                         Right unitSaves -> buildStatBlock ss (M.insert "Saves" (SavesType unitSaves) map)
                                                                                     

checkStatInputs :: [UnitInput] -> Either String [(String, StatBlock)]
checkStatInputs [] = Right []
checkStatInputs ((name, stats) : us) = case buildStatBlock stats M.empty of
                                             Left errorMsg -> Left (errorMsg ++ name ++ ".")
                                             Right statBlock -> case checkStatInputs us of
                                                                     Left errorMsg -> Left errorMsg
                                                                     Right statList -> Right ((name, statBlock) : statList)

duplicateUnitName :: [UnitInput] -> S.Set String -> Maybe String
duplicateUnitName [] _ = Nothing
duplicateUnitName ((name, _) : us) set = if S.member name set then Just ("Duplicate unit name " ++ name ++ ".")
                                                              else duplicateUnitName us (S.insert name set)

-- TO DO: Unificar esto con la función de arriba, por favor
duplicateTeamName :: [Team] -> S.Set String -> Maybe String
duplicateTeamName [] _ = Nothing
duplicateTeamName ((name, _) : ts) set = if S.member name set then Just ("Duplicate team name " ++ name ++ ".")
                                                              else duplicateTeamName ts (S.insert name set)


convertStatInputs :: [UnitInput] -> Either String (M.Map String StatBlock)
convertStatInputs units = case duplicateUnitName units S.empty of
                               Just errorMsg -> Left errorMsg
                               Nothing -> case checkStatInputs units of
                                               Left errorMsg -> Left errorMsg
                                               Right unitList -> Right (M.fromList unitList)



shiftUnits :: [Team] -> Coordinate -> [Team]
shiftUnits teams offset = map 
                          (\(team, units) -> (team, map 
                                                    (\(name, n, positions) -> (name, n, map (\(col, row) -> (col + fst offset, row + snd offset)) positions)) 
                                                    units)) 
                          teams

invalidNameInTeam :: [(String, Int, [Coordinate])] -> (M.Map String StatBlock) -> Maybe String
invalidNameInTeam [] _ = Nothing
invalidNameInTeam ((name, _, _) : us) statMap = if M.member name statMap then invalidNameInTeam us statMap
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


placeTeam :: Board -> [(String, Int, [Coordinate])] -> Int -> Either String Board
placeTeam board [] _ = Right board
placeTeam board ((name, amount, positions) : us) index = if amount /= length positions 
                                                            then Left ("Mismatched amount and coordinate amount in unit " ++ name ++ " in team ")
                                                            else case placeTeamUnits board positions index of
                                                                      Left errorMsg -> Left (errorMsg ++ name ++ " in team ")
                                                                      Right newBoard -> placeTeam newBoard us (index + length positions)


buildTeamUnits :: (M.Map String StatBlock) -> String -> (String, Int, [Coordinate]) -> [Unit]
buildTeamUnits statMap team (name, amount, positions) = map (\pos -> Mob MobUnit {name = name, team = team, position = pos, statBlock = statMap M.! name }) positions

buildTeam :: (M.Map String StatBlock) -> String -> [(String, Int, [Coordinate])] -> V.Vector Unit
buildTeam statMap team units = V.fromList (concat (map (buildTeamUnits statMap team) units))


createUnits :: Board -> (M.Map String StatBlock) -> [Team] -> V.Vector Unit -> Either String (Board, V.Vector Unit)
createUnits board _ [] units = Right (board, units)
createUnits board statMap ((teamName, positions) : ts) units = case invalidNameInTeam positions statMap of
                                                                    Just errorMsg -> Left (errorMsg ++ teamName ++ ".")
                                                                    Nothing -> case placeTeam board positions (length units) of
                                                                                    Left errorMsg -> Left (errorMsg ++ teamName ++ ".")
                                                                                    Right newBoard -> createUnits newBoard statMap ts (units V.++ buildTeam statMap teamName positions)

placeUnits :: Board -> [UnitInput] -> [Team] -> Coordinate -> Either String (Board, V.Vector Unit)
placeUnits board units teams offset = case convertStatInputs units of
                                           Left errorMsg -> Left errorMsg
                                           Right statBlocks -> case duplicateTeamName teams S.empty of
                                                                    Just errorMsg -> Left errorMsg
                                                                    Nothing -> createUnits board statBlocks (shiftUnits teams offset) V.empty
                                                                    
