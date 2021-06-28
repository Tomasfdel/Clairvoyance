module CommandParserTypes where

data Direction = DirUp | DirUpRight | DirRight | DirDownRight | DirDown | DirDownLeft | DirLeft | DirUpLeft
  deriving (Show)

data Movement = Position (Int, Int) | Path [Direction]
  deriving (Show)
  
data Target = Index Int | Description String String Int
  deriving (Show)

data Command = Next | Move Movement | Attack Target | Kill Target
  deriving (Show)
