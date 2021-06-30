module Commands.Types where

data Direction
  = DirUp
  | DirUpRight
  | DirRight
  | DirDownRight
  | DirDown
  | DirDownLeft
  | DirLeft
  | DirUpLeft
  deriving (Show)

data Movement
  = Position (Int, Int)
  | Path [Direction]
  deriving (Show)

data Target
  = Index Int
  | Description String String Int
  deriving (Show)

data Printable
  = PBoard
  | PUnit Target
  deriving (Show)

data Command
  = Next
  | Show Printable
  | Move Target Movement
  | Attack Target Int Int
  | Kill Target
  deriving (Show)
