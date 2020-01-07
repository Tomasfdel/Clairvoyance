module GameAI where

data GameAI s = Idle
              | Action (s -> s)
              | GameAI s :-: GameAI s
              | Choose [GameAI s] (s -> [GameAI s] -> GameAI s)
              | Fix (GameAI s -> GameAI s)
 --           | Atomic (GameAI s)
 --           | Interleave [GameAI s]
              | If (s -> Bool) (GameAI s) (GameAI s)

instance Show (GameAI s) where
	show Idle = "Idle"
	show (Action a) = "Action"
	show (a1 :-: a2) = (show a1) ++ ":-:" ++ (show a2)
	show (Choose as select) = "Choose " ++ (show as)
	show (Fix _) = "Fix"
	-- Atomic
	-- Interleave
	show (If cond at af) = "(" ++ "If " ++ (show at) ++ " " ++ (show af) ++ ")"
	

perform :: GameAI s -> s -> s
perform Idle state = state
perform (Action a) state = a state
perform (a1 :-: a2) state = perform a2 (perform a1 state)
perform (Choose as select) state = perform (select state as) state
perform (Fix f) state = perform (f (Fix f)) state
-- Atomic
-- Interleave
perform (If cond at af) state = if cond state then perform at state
                                              else perform af state

step :: GameAI s -> s -> (GameAI s, s)
step Idle state = (Idle, state)
step (Action a) state = (Idle, a state)
step (Idle :-: a2) state = step a2 state
step (a1 :-: a2) state = let (newA1, newState) = step a1 state
                          in (newA1 :-: a2, newState)
step (Choose as select) state = step (select state as) state
step (Fix f) state = step (f (Fix f)) state
-- Atomic
-- Interleave
step (If cond at af) state = if cond state then step at state
                                           else step af state
