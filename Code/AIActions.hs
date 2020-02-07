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
                                     in getAttackRange currAttack
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

searchTargets :: Board -> (V.Vector Unit) -> Int -> (Unit -> Bool) -> (V.Vector (V.Vector Bool)) -> (Seq.Seq (Coordinate, Float)) -> [Unit]
searchTargets board units maxR pred visited queue = if null queue then [] 
                                                                  else let ((), (newVisited, revCoords)) = runState (findNewTiles board maxR (Seq.index queue 0)) (visited, [])
                                                                           newCoords = reverse revCoords
                                                                           targets = map (\(Unit index) -> units V.! index) (filter (applyUnitPredicate units pred) (map (coordToTile board) newCoords))
                                                                        in targets ++ (searchTargets board units maxR pred newVisited ((Seq.drop 1 queue) Seq.>< (Seq.fromList newCoords)))

unitsInRange :: GameState -> Int -> (Unit -> Bool) -> Int -> (V.Vector Unit)
unitsInRange gameState index predicate range = let Mob current = (units gameState) V.! index
                                                in V.fromList (searchTargets (board gameState) (units gameState) range predicate (createSearchBoard (board gameState)) (Seq.singleton (position current, 0))) 

evalCondition :: GameState -> Int -> Condition -> Bool
evalCondition gameState index (UnitCount unitDesc (Comparison comp)) = let unitPredicate = evalUnitDesc gameState index unitDesc
                                                                           targets = V.filter unitPredicate (units gameState)
                                                                        in comp (V.length targets)
evalCondition gameState index (UnitRange unitDesc range) = let unitPredicate = evalUnitDesc gameState index unitDesc
                                                               targets = unitsInRange gameState index unitPredicate (evalRange gameState index range)
                                                            in not (V.null targets)
evalCondition gameState index (SpecificUnitRange team name id range) = let unitPredicate = (\u -> getTeam u == team && getName u == name && getIdentifier u == id)
                                                                           targets = unitsInRange gameState index unitPredicate (evalRange gameState index range)
                                                                        in not (V.null targets)
evalCondition gameState index (UnitRangeCount unitDesc range (Comparison comp)) = let unitPredicate = evalUnitDesc gameState index unitDesc
                                                                                      targets = unitsInRange gameState index unitPredicate (evalRange gameState index range)
                                                                                   in comp (V.length targets)
evalCondition gameState index (TotalTurn (Comparison comp)) = comp (turnCount gameState)
evalCondition gameState index (Not cond) = not (evalCondition gameState index cond)
evalCondition gameState index (And c1 c2) = evalCondition gameState index c1 && evalCondition gameState index c2
evalCondition gameState index (Or c1 c2) = evalCondition gameState index c1 || evalCondition gameState index c2

-- TO DO: Revisar si me conviene que sea un dieRoll o que sea una tripla
rollDice :: DieRoll -> IO (Int)
rollDice dieRoll = do results <- sequence (replicate (dieAmount dieRoll) (randomRIO (1, (dieValue dieRoll))))
                      return (modifier dieRoll + sum results)


updateIfDead :: Board -> Unit -> Board
updateIfDead board (Mob unit) = if not (unitIsAlive (Mob unit)) then let (unitCol, unitRow) = position unit
                                                                         newRow = (board V.! unitRow) V.// [(unitCol, Empty)]
                                                                         newBoard = board V.// [(unitRow, newRow)]
                                                                      in newBoard
                                                                else board

-- TO DO: Ver de hacer funciones de update units
-- TO DO: Ver si quiero precalcular el daño o lo hago solo si el ataque pega (en cuyo caso, ver cómo escribirlo lindo)
-- TO DO: Checkear cuando atacan a una unidad si está RIP
-- TO DO: Sacar el IO de todo esto, para algo agarraste un randomGen al principio
-- TO DO: Arreglar el problema del damageRoll al que le tengo que hacer el max adentro de la resta, queda espantoso.
resolveAttack :: GameState -> Int -> Int -> IO (GameState)
resolveAttack state attackInd defendInd = let Mob attacker = (units state) V.! attackInd
                                              newAttTargets = defendInd : (targets attacker)
                                              newAttacker = attacker { targets = newAttTargets }
                                              newUnits = (units state) V.// [(attackInd, Mob newAttacker)]
                                              Mob defender = newUnits V.! defendInd
                                           in do attackRoll <- rollDice (DieRoll {dieAmount = 1, dieValue = 20, modifier = (\(_, modifier, _) -> modifier) (attack (statBlock newAttacker)) })
                                                 damageRoll <- (rollDice ((\(_, _, damageRoll) -> damageRoll) (attack (statBlock newAttacker))))
                                                 if attackRoll >= armorClass (statBlock defender) then let damageDealt = max 1 damageRoll
                                                                                                           newDefender = defender { statBlock = (statBlock defender) { healthPoints = healthPoints (statBlock defender) - damageDealt } }
                                                                                                           finalUnits = newUnits V.// [(defendInd, Mob newDefender)]
                                                                                                           finalBoard = updateIfDead (board state) (Mob newDefender)
                                                                                                        in do putStrLn "Attack Successful!\n"
                                                                                                              return (state { board = finalBoard, units = finalUnits })
                                                                                                  else do putStrLn "Attack Failed!\n"
                                                                                                          return (state { units = newUnits })

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
sortByAdjective :: GameState -> Int -> [Unit] -> Adjective -> [Int]
sortByAdjective state index searchUnits Closest = map (unitToInt state) searchUnits
sortByAdjective state index searchUnits Furthest = reverse (sortByAdjective state index searchUnits Closest)
sortByAdjective state index searchUnits LeastInjured = map (unitToInt state) (L.sortBy (O.comparing (\(Mob unit) -> healthPoints (statBlock unit))) searchUnits)
sortByAdjective state index searchUnits MostInjured = reverse (sortByAdjective state index searchUnits LeastInjured)
sortByAdjective state index searchUnits Last = let unitSet = S.fromList (map (unitToInt state) searchUnits)
                                                   targetHistory = getTargets ((units state) V.! index)
                                                in findUnitsInHistory unitSet targetHistory


evalMoveAction :: GameState -> Int -> MoveAction -> IO(GameState, Bool)
evalMoveAction state _ _ = return (state, True)
-- ~ evalMoveAction gameState index (Approach target) = 
                                              

evalStandardAction :: GameState -> Int -> StandardAction -> IO(GameState, Bool)
-- ~ evalStandardAction state _ _ = (state, True)
evalStandardAction gameState index (AttackAction target) = case target of
                                                                Self -> do newState <- resolveAttack gameState index index
                                                                           return (newState, True)
                                                                Specific team name id -> let predicate = (\u -> getTeam u == team && getName u == name && getIdentifier u == id)
                                                                                             attackRange = getAttackRange (attack (getStatBlock ((units gameState) V.! index)))
                                                                                             searchResults = unitsInRange gameState index predicate attackRange
                                                                                          in if not (V.null searchResults) then do newState <- resolveAttack gameState index (unitToInt gameState (V.head searchResults))
                                                                                                                                   return (newState, True)
                                                                                                                           else return (gameState, False)
                                                                Description adjective unitDesc -> let unitPred = evalUnitDesc gameState index unitDesc
                                                                                                      attackRange = getAttackRange (attack (getStatBlock ((units gameState) V.! index)))
                                                                                                      searchResults = unitsInRange gameState index unitPred attackRange
                                                                                                      orderedTargets = sortByAdjective gameState index (V.toList searchResults) adjective 
                                                                                                   in if not (null orderedTargets) then do newState <- resolveAttack gameState index (head orderedTargets)
                                                                                                                                           return (newState, True)
                                                                                                                                   else return (gameState, False)

-- TO DO: Ver cómo estructurar las default choices. De momento si no puede hacer una acción, no hace nada.

evalFullAction :: GameState -> Int -> FullAction -> IO(GameState, Bool)
evalFullAction state _ _ = return (state, True)
-- ~ evalFullAction gameState index (FullAttackAction target) = 


-- TO DO: Dejar de hacer esto tail recursive cuando vea cómo meter State en todo esto.
evalTurn :: GameState -> Int -> [TurnAction] -> Bool -> IO (GameState, Bool)
evalTurn gameState _ [] result = return (gameState, result)
evalTurn gameState _ (Pass : as) result = return (gameState, result)
evalTurn gameState index ((Move ma) : as) result = do (newState, maResult) <- evalMoveAction gameState index ma 
                                                      evalTurn newState index as (result && maResult)
evalTurn gameState index ((Standard sa) : as) result = do (newState, saResult) <- evalStandardAction gameState index sa 
                                                          evalTurn newState index as (result && saResult)
evalTurn gameState index ((Full fa) : as) result = do (newState, faResult) <- evalFullAction gameState index fa 
                                                      evalTurn newState index as (result && faResult)


aiStep :: GameState -> Int -> Action -> IO(GameState, Action, Bool)
aiStep gameState index None = return (gameState, None, False)
aiStep gameState index (Turn ts) = do (newState, success) <- evalTurn gameState index ts True
                                      let newAction = if success then None else (Turn ts)
                                       in return (newState, newAction, success)
aiStep gameState index (If cond tAct fAct) = let nextAction = if evalCondition gameState index cond then tAct else fAct
                                              in aiStep gameState index nextAction
aiStep gameState index (Cons first second) = do response <- aiStep gameState index first
                                                case response of
                                                     (newState, None, playedTurn) -> if playedTurn then return (newState, second, playedTurn)
                                                                                                   else aiStep gameState index second
                                                     (newState, newFirst, playedTurn) -> return (newState, Cons newFirst second, playedTurn)
aiStep gameState index (While cond action) = if evalCondition gameState index cond then aiStep gameState index (Cons action (While cond action))
                                                                                   else return (gameState, None, False)
