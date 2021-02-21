module LexerTokens where

data LexerToken =
  LTMap            |
  LTLayout         |
  LTRectangle      |
  LTOutline        |
  LTObstacles      |
  LTDirUp          |
  LTDirRight       |
  LTDirDown        |
  LTDirLeft        |
  LTNat Int        |
  LTDie (Int ,Int) |
  LTMult Int       |
  LTSym Char       |
  LTVar String     |
  LTUnit           |
  LTHP             |
  LTInitiative     |
  LTSpeed          |
  LTAC             |
  LTAttack         |
  LTFullAttack     |
  LTMelee          |
  LTRanged         |
  LTAI             |
  LTPass           |
  LTIf             |
  LTThen           |
  LTElse           |
  LTWhile          |
  LTNot            |
  LTAnd            |
  LTOr             |
  LTApproach       |
  LTAlly           |
  LTEnemy          |
  LTSelf           |
  LTClosest        |
  LTFurthest       |
  LTMost           |
  LTLeast          |
  LTInjured        |
  LTLast           |
  LTCount          |
  LTIn             |
  LTRange          |
  LTTotal          |
  LTTurn           |
  LTTeam          
  deriving (Eq,Show)
