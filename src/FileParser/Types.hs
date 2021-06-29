module FileParser.Types where

data Direction = DirUp | DirRight | DirDown | DirLeft
  deriving (Show)

data Layout = Rectangle Int Int | Outline [(Int, Direction)]
  deriving (Show)

type Coordinate = (Int, Int)

type Obstacle = (Coordinate, Coordinate)

type Map = (Layout, [Obstacle])

data DieRoll = DieRoll
  { dieAmount :: Int,
    dieValue :: Int,
    modifier :: Int
  }
  deriving (Show)

data AttackRange = Melee | Ranged Int
  deriving (Show)

type AttackDesc = (AttackRange, Int, DieRoll)

data StatInput
  = HP Int
  | Initiative Int
  | Speed Int
  | AC Int
  | Attack [AttackDesc]
  | FullAttack [AttackDesc]
  deriving (Show)

data UnitInput
  = MobInput (String, [StatInput])
  | PlayerInput (String, Int)

data Range
  = IntR Int
  | MeleeR
  | AttackR
  | FullAttackR
  | SpeedR
  | Sum Range Range
  | Prod Range Int
  deriving (Show)

data IntComparison = Comparison (Int -> Bool)

instance Show IntComparison where
  show (Comparison f) = "IntComparison"

data Condition
  = UnitCount UnitDesc IntComparison
  | UnitRange UnitDesc Range
  | SpecificUnitRange String String Int Range
  | UnitRangeCount UnitDesc Range IntComparison
  | TotalTurn IntComparison
  | Not Condition
  | And Condition Condition
  | Or Condition Condition
  deriving (Show)

data UnitDesc
  = Ally
  | Enemy
  | Team String
  | TeamUnit String String
  deriving (Show)

data Adjective
  = Closest
  | Furthest
  | Last
  deriving (Show)

data Target
  = Self
  | Specific String String Int
  | Description Adjective UnitDesc
  deriving (Show)

data MoveAction
  = Approach Target
  | Disengage
  deriving (Show)

data StandardAction = AttackAction Target
  deriving (Show)

data FullAction = FullAttackAction Target
  deriving (Show)

data TurnAction
  = Pass
  | Move MoveAction
  | Standard StandardAction
  | Full FullAction
  deriving (Show)

data Action
  = None
  | Turn [TurnAction]
  | If Condition Action Action
  | While Condition Action
  | Cons Action Action
  deriving (Show)

type AIInput = (String, Action)

type Team = (String, [(String, String, Int, [Coordinate])])

type Game = (Map, [UnitInput], [AIInput], [Team])
