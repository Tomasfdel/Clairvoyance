module AIActions where

import ParserTypes 
import BoardGeneration
import StatBlockGeneration
import UnitPlacement
import GameState
import Control.Monad.State
import System.Random
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Vector as V


buildAImap :: [AIInput] -> (M.Map String Action) -> Either String (M.Map String Action)
buildAImap [] aiMap = Right aiMap
buildAImap ((name, ai) : ais) aiMap = if M.member name aiMap then Left ("Dupĺicate AI name " ++ name ++ ".")
                                                             else buildAImap ais (M.insert name ai aiMap)

checkAInames :: [AIInput] -> Either String (M.Map String Action)
checkAInames ais = buildAImap ais M.empty


evalUnitDesc :: GameState -> Int -> UnitDesc -> (Unit -> Bool)
evalUnitDesc gameState index Ally = \u -> (getTeam u) == getTeam ((units gameState) V.! index)
evalUnitDesc gameState index Enemy = \u -> (getTeam u) /= getTeam ((units gameState) V.! index)
evalUnitDesc gameState index (Team name) = \u -> (getTeam u) == name
evalUnitDesc gameState index (TeamUnit team name) = \u -> (getTeam u) == name && (getName u) == name

getAttackRange :: AttackDesc -> Int
getAttackRange (Melee, _, _) = 1
getAttackRange (Ranged r, _, _) = r

evalRange :: GameState -> Int -> Range -> Int
evalRange gameState index (IntR n) = n
evalRange gameState index MeleeR = 1
evalRange gameState index AttackR = let Mob current = (units gameState) V.! index
                                        currAttack = attack (statBlock current)
                                     in minimum (map getAttackRange currAttack)
evalRange gameState index FullAttackR = let Mob current = (units gameState) V.! index
                                            currFullAttack = fullAttack (statBlock current)
                                         in minimum (map getAttackRange currFullAttack) 
evalRange gameState index SpeedR = let Mob current = (units gameState) V.! index
                                    in speed (statBlock current)
evalRange gameState index (Sum r1 r2) = (evalRange gameState index r1) + (evalRange gameState index r2)
evalRange gameState index (Prod r n) = (evalRange gameState index r) * n


createSearchBoard :: Board -> V.Vector (V.Vector Bool)
createSearchBoard board = let height = V.length board
                              width  = V.length (V.head board)
                           in V.replicate height (V.replicate width False)

-- TO DO: Ver si puedo pasar esto a do notation
findSideTile :: Board -> Int -> Coordinate -> Float -> Int -> Int -> State (V.Vector (V.Vector Bool), [(Coordinate, Float)]) ()
findSideTile board maxR (col, row) distance colDiff rowDiff = state $ \(visited, found) -> if floor (distance + 1) <= maxR &&
                                                                                              validCoord board (col + colDiff, row + rowDiff) &&
                                                                                              not ((visited V.! (row + rowDiff)) V.! (col + colDiff)) &&
                                                                                              (board V.! (row + rowDiff)) V.! (col + colDiff) /= Wall 
                                                                                              then let newVisRow = (visited V.! (row + rowDiff)) V.// [(col + colDiff, True)]
                                                                                                       newVisited = visited V.// [(row + rowDiff, newVisRow)]
                                                                                                       newFound = ((col + colDiff, row + rowDiff), distance + 1) : found
                                                                                                    in ((), (newVisited, newFound))
                                                                                              else ((), (visited, found))
                                                                    

findDiagonalTile :: Board -> Int -> Coordinate -> Float -> Int -> Int -> State (V.Vector (V.Vector Bool), [(Coordinate, Float)]) ()
findDiagonalTile board maxR (col, row) distance colDiff rowDiff = state $ \(visited, found) -> if floor (distance + 1.5) <= maxR &&
                                                                                                  validCoord board (col + colDiff, row + rowDiff) &&
                                                                                                  not ((visited V.! (row + rowDiff)) V.! (col + colDiff)) &&
                                                                                                  (board V.! (row + rowDiff)) V.! (col + colDiff) /= Wall &&
                                                                                                  any (\((c, r), _) -> c == (col + colDiff) && r == row) found &&
                                                                                                  any (\((c, r), _) -> c == col && r == (row + rowDiff)) found
                                                                                                  then let newVisRow = (visited V.! (row + rowDiff)) V.// [(col + colDiff, True)]
                                                                                                           newVisited = visited V.// [(row + rowDiff, newVisRow)]
                                                                                                           newFound = ((col + colDiff, row + rowDiff), distance + 1.5) : found
                                                                                                        in ((), (newVisited, newFound))
                                                                                                  else ((), (visited, found))
                                                                        

findNewTiles :: Board -> Int -> (Coordinate, Float) -> State (V.Vector (V.Vector Bool), [(Coordinate, Float)]) ()
findNewTiles board maxR (pos, distance) = do findSideTile board maxR pos distance 0 1
                                             findSideTile board maxR pos distance 0 (-1)
                                             findSideTile board maxR pos distance 1 0
                                             findSideTile board maxR pos distance (-1) 0
                                             findDiagonalTile board maxR pos distance 1 1
                                             findDiagonalTile board maxR pos distance 1 (-1)
                                             findDiagonalTile board maxR pos distance (-1) 1
                                             findDiagonalTile board maxR pos distance (-1) (-1)
                                             return ()

coordToTile :: Board -> (Coordinate, Float) -> Tile
coordToTile board ((col, row), _) = (board V.! row) V.! col

applyUnitPredicate :: (V.Vector Unit) -> (Unit -> Bool) -> Tile -> Bool
applyUnitPredicate units pred (Unit index) = pred (units V.! index)
applyUnitPredicate _ _ _ = False

searchTargets :: Board -> (V.Vector Unit) -> Int -> (Unit -> Bool) -> (V.Vector (V.Vector Bool)) -> (Seq.Seq (Coordinate, Float)) -> [Int]
searchTargets board units maxR pred visited queue = if null queue then [] 
                                                                  else let ((), (newVisited, revCoords)) = runState (findNewTiles board maxR (Seq.index queue 0)) (visited, [])
                                                                           newCoords = reverse revCoords
                                                                           targets = map (\(Unit index) -> index) (filter (applyUnitPredicate units pred) (map (coordToTile board) newCoords))
                                                                        in targets ++ (searchTargets board units maxR pred newVisited ((Seq.drop 1 queue) Seq.>< (Seq.fromList newCoords)))

unitsInRange :: GameState -> Int -> (Unit -> Bool) -> Int -> [Int]
unitsInRange gameState index predicate range = let Mob current = (units gameState) V.! index
                                                in searchTargets (board gameState) (units gameState) range predicate (createSearchBoard (board gameState)) (Seq.singleton (position current, 0))

evalCondition :: GameState -> Int -> Condition -> Bool
evalCondition gameState index (UnitCount unitDesc (Comparison comp)) = let unitPredicate = evalUnitDesc gameState index unitDesc
                                                                           targets = V.filter unitPredicate (units gameState)
                                                                        in comp (V.length targets)
evalCondition gameState index (UnitRange unitDesc range) = let unitPredicate = evalUnitDesc gameState index unitDesc
                                                               targets = unitsInRange gameState index unitPredicate (evalRange gameState index range)
                                                            in not (null targets)
evalCondition gameState index (SpecificUnitRange team name id range) = let unitPredicate = (\u -> getTeam u == team && getName u == name && getIdentifier u == id)
                                                                           targets = unitsInRange gameState index unitPredicate (evalRange gameState index range)
                                                                        in not (null targets)
evalCondition gameState index (UnitRangeCount unitDesc range (Comparison comp)) = let unitPredicate = evalUnitDesc gameState index unitDesc
                                                                                      targets = unitsInRange gameState index unitPredicate (evalRange gameState index range)
                                                                                   in comp (length targets)
evalCondition gameState index (TotalTurn (Comparison comp)) = comp (turnCount gameState)
evalCondition gameState index (Not cond) = not (evalCondition gameState index cond)
evalCondition gameState index (And c1 c2) = evalCondition gameState index c1 && evalCondition gameState index c2
evalCondition gameState index (Or c1 c2) = evalCondition gameState index c1 || evalCondition gameState index c2

rollDie :: Int -> Int -> State GameState Int
rollDie minVal maxVal = state $ \gameState -> let generator = randomGen gameState
                                                  (result, newGen) = randomR (minVal, maxVal) generator
                                               in (result, gameState { randomGen = newGen })

-- TO DO: Revisar si me conviene que sea un dieRoll o que sea una tripla
rollDice :: DieRoll -> State GameState Int
rollDice dieRoll = do results <- sequence (replicate (dieAmount dieRoll) (rollDie 1 (dieValue dieRoll)))
                      return (modifier dieRoll + sum results)

updateBoard :: Coordinate -> Tile -> State GameState ()
updateBoard (col, row) newTile = modify (\gameState -> let newRow = ((board gameState) V.! row) V.// [(col, newTile)]
                                                           newBoard = (board gameState) V.// [(row, newRow)]
                                                        in gameState { board = newBoard } )
                                                    

updateIfDead :: Int -> State GameState ()
updateIfDead index = do gameState <- get
                        let unit = (units gameState) V.! index
                         in if not (unitIsAlive unit) then do updateBoard (getPosition unit) Empty
                                                              return ()
                                                      else return ()

rollAttack :: AttackDesc -> State GameState (Int, Int)
rollAttack (_, mod, damage) = do attackRoll <- rollDice (DieRoll { dieAmount = 1, dieValue = 20, modifier = mod } )
                                 damageRoll <- rollDice damage
                                 return (attackRoll, max 1 damageRoll)

checkAttackHit :: Int -> (Int, Int) -> State GameState ()
checkAttackHit defendInd (attackRoll, damageRoll) = do (Mob defender) <- gets (\gameState -> (units gameState) V.! defendInd)
                                                       if attackRoll >= armorClass (statBlock defender) 
                                                          then modify (\gameState -> let newDefender = defender { statBlock = (statBlock defender) { healthPoints = healthPoints (statBlock defender) - damageRoll } }
                                                                                         newUnits = (units gameState) V.// [(defendInd, Mob newDefender)] 
                                                                                      in gameState { units = newUnits })
                                                          else return ()

-- TO DO: Ver de hacer funciones de update units
-- TO DO: Ver si quiero precalcular el daño o lo hago solo si el ataque pega (en cuyo caso, ver cómo escribirlo lindo)
-- TO DO: Checkear cuando atacan a una unidad si está RIP
-- TO DO: Sacar el IO de todo esto, para algo agarraste un randomGen al principio
-- TO DO: Arreglar el problema del damageRoll al que le tengo que hacer el max adentro de la resta, queda espantoso.
resolveAttack :: Int -> Int -> (StatBlock -> [AttackDesc]) -> State GameState ()
resolveAttack attackInd defendInd attackType = do (Mob attacker) <- gets (\gameState -> (units gameState) V.! attackInd)
                                                  modify (\gameState -> let newAttacker = attacker { targets = defendInd : (targets attacker) }
                                                                         in gameState { units = (units gameState) V.// [(attackInd, Mob newAttacker)] })
                                                  attackRolls <- mapM rollAttack (attackType (statBlock attacker))
                                                  mapM_ (checkAttackHit defendInd) attackRolls
                                                  updateIfDead defendInd


unitToInt :: GameState -> Unit -> Int
unitToInt state (Mob unit) = let (col, row) = position unit
                                 Unit index = ((board state) V.! row) V.! col
                              in index

intToUnit :: GameState -> Int -> Unit
intToUnit state index = (units state) V.! index

findUnitsInHistory :: (S.Set Int) -> [Int] -> [Int]
findUnitsInHistory _ [] = []
findUnitsInHistory unitSet (index : is) = if S.member index unitSet then index : (findUnitsInHistory (S.delete index unitSet) is)
                                                                    else findUnitsInHistory unitSet is

-- TO DO: COnsiderar unificar si uso Ints o Units para las funciones
sortByAdjective :: GameState -> Int -> [Int] -> Adjective -> [Int]
sortByAdjective _ _ searchUnits Closest = searchUnits
sortByAdjective state index searchUnits Furthest = reverse (sortByAdjective state index searchUnits Closest)
sortByAdjective state index searchUnits LeastInjured = L.sortBy (O.comparing (\index -> healthPoints (getStatBlock ((units state) V.! index)))) searchUnits
sortByAdjective state index searchUnits MostInjured = reverse (sortByAdjective state index searchUnits LeastInjured)
sortByAdjective state index searchUnits Last = let targetHistory = getTargets ((units state) V.! index)
                                                in findUnitsInHistory (S.fromList searchUnits) targetHistory

evalTarget :: GameState -> Int -> Int -> Target -> [Int]
evalTarget _ index _ Self = [index]
evalTarget state index range (Specific team name id) = let predicate = (\u -> getTeam u == team && getName u == name && getIdentifier u == id)
                                                        in unitsInRange state index predicate range
evalTarget state index range (Description adjective unitDesc) = let unitPred = evalUnitDesc state index unitDesc
                                                                    searchResults = unitsInRange state index unitPred range
                                                                 in sortByAdjective state index searchResults adjective 


-- TO DO: Ver cómo estructurar las default choices. De momento si no puede hacer una acción, no hace nada.
-- TO DO: Ver cómo unificar la evaluación de targets en las distintas acciones
evalAction :: Int -> TurnAction -> State GameState Bool 
evalAction _ (Move (Approach target)) = return True
evalAction index (Standard (AttackAction target)) = do gameState <- get
                                                       let attackRange = minimum (map getAttackRange (attack (getStatBlock ((units gameState) V.! index))))
                                                           targets = evalTarget gameState index attackRange target
                                                        in if not (null targets) then do resolveAttack index (head targets) attack
                                                                                         return True
                                                                                 else return False
evalAction index (Full (FullAttackAction target)) = do gameState <- get
                                                       let attackRange = minimum (map getAttackRange (fullAttack (getStatBlock ((units gameState) V.! index))))
                                                           targets = evalTarget gameState index attackRange target
                                                        in if not (null targets) then do resolveAttack index (head targets) fullAttack
                                                                                         return True
                                                                                 else return False

-- TO DO: Dejar de hacer esto tail recursive cuando vea cómo meter State en todo esto.
evalTurn :: Int -> [TurnAction] -> State GameState Bool
evalTurn _ [] = return True
evalTurn _ (Pass : as) = return True
evalTurn index (action : as) = do result <- evalAction index action 
                                  restResult <- evalTurn index as
                                  return (result && restResult)

aiStep :: Int -> Action -> State GameState (Action, Bool)
aiStep index None = return (None, False)
aiStep index (Turn ts) = do success <- evalTurn index ts
                            let newAction = if success then None else (Turn ts)
                             in return (newAction, success)
aiStep index (If cond tAct fAct) = do gameState <- get
                                      let nextAction = if evalCondition gameState index cond then tAct else fAct
                                       in aiStep index nextAction
aiStep index (Cons first second) = do gameState <- get
                                      response <- aiStep index first
                                      case response of
                                           (None, playedTurn) -> if playedTurn then return (second, True)
                                                                               else do put gameState
                                                                                       aiStep index second
                                           (newFirst, playedTurn) -> return (Cons newFirst second, playedTurn)
aiStep index (While cond action) = do gameState <- get
                                      if evalCondition gameState index cond then aiStep index (Cons action (While cond action))
                                                                            else return (None, False)
