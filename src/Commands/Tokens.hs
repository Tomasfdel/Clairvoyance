module Commands.Tokens where

data LexerToken
  = LTNext
  | LTShow
  | LTBoard
  | LTMove
  | LTUp
  | LTUpRight
  | LTRight
  | LTDownRight
  | LTDown
  | LTDownLeft
  | LTLeft
  | LTUpLeft
  | LTAttack
  | LTKill
  | LTNat Int
  | LTSym Char
  | LTVar String
  deriving (Eq, Show)
