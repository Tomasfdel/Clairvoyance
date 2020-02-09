module StatBlockGeneration where

import ParserTypes
import BoardGeneration
import qualified Data.Map as M
import qualified Data.Set as S

-- TO DO: Cambiar los error msg para que cada función agregue la parte que le corresponde y no sólo el nombre del team, unit o lo que sea.

data StatBlock = StatBlock {
                     healthPoints :: Int, 
                     initiative :: Int,
                     speed :: Int,
                     armorClass :: Int,
                     attack :: [AttackDesc],
                     fullAttack :: [AttackDesc]} 
                 deriving Show
                 
statBlockSize :: Int
statBlockSize = 6


data StatTypes = IntType Int              |
                 AttacksType [AttackDesc] 
                 deriving Show


statBlockFromMap :: M.Map String StatTypes -> StatBlock
statBlockFromMap map = let (IntType healthPoints)   = map M.! "HP"
                           (IntType initiative)     = map M.! "Initiative"
                           (IntType speed)          = map M.! "Speed"
                           (IntType armorClass)     = map M.! "AC"
                           (AttacksType attack)     = map M.! "Attack"
                           (AttacksType fullAttack) = map M.! "FullAttack"
                        in StatBlock { healthPoints = healthPoints,
                                       initiative = initiative,
                                       speed = speed,
                                       armorClass = armorClass,
                                       attack = attack,
                                       fullAttack = fullAttack}

validAttack :: AttackDesc -> Bool
validAttack (_, _, roll) = (dieAmount roll) >= 0 && (dieValue roll) > 0

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
buildStatBlock ((Attack ns) : ss) map = if M.member "Attack" map then Left "Duplicate Attack field in unit "
                                                                 else if not (all validAttack ns) then Left "Invalid Attack in unit "
                                                                                                  else buildStatBlock ss (M.insert "Attack" (AttacksType ns) map)
buildStatBlock ((FullAttack ns) : ss) map = if M.member "FullAttack" map then Left "Duplicate Full Attack field in unit "
                                                                          else if not (all validAttack ns) then Left "Invalid Full Attack in unit "
                                                                                                                else buildStatBlock ss (M.insert "FullAttack" (AttacksType ns) map)
                                                                                     

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


convertStatInputs :: [UnitInput] -> Either String (M.Map String StatBlock)
convertStatInputs units = case duplicateUnitName units S.empty of
                               Just errorMsg -> Left errorMsg
                               Nothing -> case checkStatInputs units of
                                               Left errorMsg -> Left errorMsg
                                               Right unitList -> Right (M.fromList unitList)
