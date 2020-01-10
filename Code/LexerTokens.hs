module LexerTokens where

data LexerToken =
  LTMap         |
  LTLayout      |
  LTRectangle   |
  LTOutline     |
  LTObstacles   |
  LTDirUp       |
  LTDirRight    |
  LTDirDown     |
  LTDirLeft     |
  LTInt Int     |
  LTSym Char    |
  LTVar String  |
  LTUnit        |
  LTHP          |
  LTInitiative  |
  LTSpeed       |
  LTAC          |
  LTAttack      |
  LTFullAttack  |
  LTMelee       |
  LTRanged      |
  LTSpells      |
  LTSaves       |
  LTFortitude   |
  LTReflex      |
  LTWill        |
  LTTeam          
  deriving (Eq,Show)
