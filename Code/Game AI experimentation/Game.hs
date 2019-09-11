import Nim
import System.IO


main :: IO()
main = do hSetBuffering stdout NoBuffering
          putStrLn "Estado inicial: "
          game startState 0
          
game :: GameState -> Int -> IO()
game state 0 = do printState state
                  if checkVictory state then putStrLn "Perdiste!"
                                        else do newState <- playerPlay state
                                                game newState 1
game state 1 = do if checkVictory state then putStrLn "Ganaste!"
                                        else do newState <- aiPlay state
                                                game newState 0
                                          
                                             
                                             
          
