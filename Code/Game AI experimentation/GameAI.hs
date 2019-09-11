module GameAI where

data GameAI s = Idle
              | Action (s -> s)
              | GameAI s :-: GameAI s
              | Choose [GameAI s] (s -> [GameAI s] -> GameAI s)
 --           | Fix (GameAI s -> GameAI s)
 --           | Atomic (GameAI s)
 --           | Interleave [GameAI s]
              | If (s -> Bool) (GameAI s) (GameAI s)

perform :: GameAI s -> s -> s
perform Idle state = state
perform (Action a) state = a state
perform (a1 :-: a2) state = perform a2 (perform a1 state)
perform (Choose as select) state = perform (select state as) state
-- Fix
-- Atomic
-- Interleave
perform (If cond at af) state = if cond state then perform at state
                                              else perform af state

