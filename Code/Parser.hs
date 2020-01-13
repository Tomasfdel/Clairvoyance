{-# OPTIONS_GHC -w #-}
module Parser where

import LexerTokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26
	= HappyTerminal (LexerToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,187) ([0,1024,0,0,0,1024,0,0,0,0,128,0,0,0,0,2048,0,0,0,0,0,2048,0,0,0,0,0,16,0,0,64,0,0,0,0,2048,0,0,0,0,0,0,64,0,0,0,0,1024,0,12288,0,0,0,0,0,2048,0,0,16128,1,0,0,0,4096,0,0,0,512,0,0,0,1024,0,0,0,1024,0,0,0,1024,0,0,0,1024,0,0,0,1024,0,0,0,1024,0,0,0,1024,0,0,64,0,0,0,0,512,0,0,8,0,0,0,8,0,0,0,0,0,0,32768,7,0,0,0,0,256,0,16384,0,0,0,0,0,4096,0,0,0,512,0,0,32,0,0,0,0,14,0,0,49152,0,0,0,49152,0,0,0,8,0,0,0,8,0,0,0,0,192,0,0,8,0,0,0,16128,1,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,192,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,256,0,0,0,1024,0,0,0,1024,0,0,0,1024,0,0,0,1024,0,0,64,0,0,0,0,16,0,0,0,1024,0,0,8,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,192,0,0,0,192,0,0,0,192,0,0,0,14,0,0,49152,0,0,0,8,0,0,0,24,0,0,0,0,0,0,0,0,192,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,512,0,0,8,0,0,0,0,0,0,0,0,256,0,0,0,128,0,0,0,4096,0,0,0,256,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,8,0,0,0,8,0,0,0,0,16384,0,0,0,0,0,0,0,16384,0,0,0,256,0,0,0,256,0,0,0,0,0,0,0,8192,0,0,0,8192,0,0,0,0,0,0,8,0,0,0,0,256,0,0,8,0,0,0,0,16384,0,0,0,256,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Game","Board","LayoutDesc","DirectionList","Direction","ObstacleList","ObstacleTail","IntRange","UnitList","StatList","UnitStat","AttackDescList","AttackDesc","AttackRange","Modifier","DieRoll","DiceExp","SavesList","UnitSave","TeamList","TeamMemberList","TeamMember","CoordinateList","Map","Layout","Rectangle","Outline","Obstacles","U","R","D","L","Int","Die","Count","Name","Unit","HP","Initiative","Speed","AC","Attack","FullAttack","Melee","Ranged","Saves","Fortitude","Reflex","Will","Team","'d'","'+'","'-'","','","';'","':'","'{'","'}'","'('","')'","%eof"]
        bit_start = st * 64
        bit_end = (st + 1) * 64
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..63]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (27) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (27) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (40) = happyShift action_7
action_2 (12) = happyGoto action_6
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (60) = happyShift action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (64) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (28) = happyShift action_11
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (53) = happyShift action_10
action_6 (23) = happyGoto action_9
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (39) = happyShift action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (60) = happyShift action_14
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_1

action_10 (39) = happyShift action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (59) = happyShift action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (29) = happyShift action_26
action_12 (30) = happyShift action_27
action_12 (6) = happyGoto action_25
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (60) = happyShift action_24
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (41) = happyShift action_17
action_14 (42) = happyShift action_18
action_14 (43) = happyShift action_19
action_14 (44) = happyShift action_20
action_14 (45) = happyShift action_21
action_14 (46) = happyShift action_22
action_14 (49) = happyShift action_23
action_14 (13) = happyGoto action_15
action_14 (14) = happyGoto action_16
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (61) = happyShift action_43
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (58) = happyShift action_42
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (59) = happyShift action_41
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (59) = happyShift action_40
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (59) = happyShift action_39
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (59) = happyShift action_38
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (59) = happyShift action_37
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (59) = happyShift action_36
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (59) = happyShift action_35
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (39) = happyShift action_34
action_24 (24) = happyGoto action_32
action_24 (25) = happyGoto action_33
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (58) = happyShift action_31
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (36) = happyShift action_30
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (36) = happyShift action_29
action_27 (7) = happyGoto action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_4

action_29 (32) = happyShift action_69
action_29 (33) = happyShift action_70
action_29 (34) = happyShift action_71
action_29 (35) = happyShift action_72
action_29 (8) = happyGoto action_68
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (57) = happyShift action_67
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (31) = happyShift action_66
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (61) = happyShift action_65
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (58) = happyShift action_64
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (38) = happyShift action_63
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (50) = happyShift action_60
action_35 (51) = happyShift action_61
action_35 (52) = happyShift action_62
action_35 (21) = happyGoto action_58
action_35 (22) = happyGoto action_59
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (47) = happyShift action_54
action_36 (48) = happyShift action_55
action_36 (15) = happyGoto action_56
action_36 (16) = happyGoto action_57
action_36 (17) = happyGoto action_53
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (47) = happyShift action_54
action_37 (48) = happyShift action_55
action_37 (16) = happyGoto action_52
action_37 (17) = happyGoto action_53
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (36) = happyShift action_51
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (36) = happyShift action_50
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (55) = happyShift action_48
action_40 (56) = happyShift action_49
action_40 (18) = happyGoto action_47
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (36) = happyShift action_46
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (41) = happyShift action_17
action_42 (42) = happyShift action_18
action_42 (43) = happyShift action_19
action_42 (44) = happyShift action_20
action_42 (45) = happyShift action_21
action_42 (46) = happyShift action_22
action_42 (49) = happyShift action_23
action_42 (13) = happyGoto action_45
action_42 (14) = happyGoto action_16
action_42 _ = happyReduce_19

action_43 (40) = happyShift action_7
action_43 (12) = happyGoto action_44
action_43 _ = happyReduce_17

action_44 _ = happyReduce_18

action_45 _ = happyReduce_20

action_46 _ = happyReduce_21

action_47 _ = happyReduce_22

action_48 (36) = happyShift action_91
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (36) = happyShift action_90
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_23

action_51 _ = happyReduce_24

action_52 _ = happyReduce_25

action_53 (36) = happyShift action_88
action_53 (37) = happyShift action_89
action_53 (55) = happyShift action_48
action_53 (56) = happyShift action_49
action_53 (18) = happyGoto action_85
action_53 (19) = happyGoto action_86
action_53 (20) = happyGoto action_87
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_32

action_55 (62) = happyShift action_84
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_26

action_57 (57) = happyShift action_83
action_57 _ = happyReduce_28

action_58 _ = happyReduce_27

action_59 (57) = happyShift action_82
action_59 _ = happyReduce_40

action_60 (59) = happyShift action_81
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (59) = happyShift action_80
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (59) = happyShift action_79
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (59) = happyShift action_78
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (39) = happyShift action_34
action_64 (24) = happyGoto action_77
action_64 (25) = happyGoto action_33
action_64 _ = happyReduce_47

action_65 (53) = happyShift action_10
action_65 (23) = happyGoto action_76
action_65 _ = happyReduce_45

action_66 (59) = happyShift action_75
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (36) = happyShift action_74
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (56) = happyShift action_73
action_68 _ = happyReduce_5

action_69 _ = happyReduce_7

action_70 _ = happyReduce_8

action_71 _ = happyReduce_9

action_72 _ = happyReduce_10

action_73 (36) = happyShift action_29
action_73 (7) = happyGoto action_105
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_3

action_75 (62) = happyShift action_104
action_75 (9) = happyGoto action_103
action_75 _ = happyReduce_11

action_76 _ = happyReduce_46

action_77 _ = happyReduce_48

action_78 (62) = happyShift action_102
action_78 (26) = happyGoto action_101
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (55) = happyShift action_48
action_79 (56) = happyShift action_49
action_79 (18) = happyGoto action_100
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (55) = happyShift action_48
action_80 (56) = happyShift action_49
action_80 (18) = happyGoto action_99
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (55) = happyShift action_48
action_81 (56) = happyShift action_49
action_81 (18) = happyGoto action_98
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (50) = happyShift action_60
action_82 (51) = happyShift action_61
action_82 (52) = happyShift action_62
action_82 (21) = happyGoto action_97
action_82 (22) = happyGoto action_59
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (47) = happyShift action_54
action_83 (48) = happyShift action_55
action_83 (15) = happyGoto action_96
action_83 (16) = happyGoto action_57
action_83 (17) = happyGoto action_53
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (36) = happyShift action_95
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (36) = happyShift action_88
action_85 (37) = happyShift action_89
action_85 (19) = happyGoto action_94
action_85 (20) = happyGoto action_87
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_31

action_87 (55) = happyShift action_48
action_87 (56) = happyShift action_49
action_87 (18) = happyGoto action_93
action_87 _ = happyReduce_36

action_88 (54) = happyShift action_92
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_39

action_90 _ = happyReduce_35

action_91 _ = happyReduce_34

action_92 (36) = happyShift action_111
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_37

action_94 _ = happyReduce_30

action_95 (63) = happyShift action_110
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_29

action_97 _ = happyReduce_41

action_98 _ = happyReduce_42

action_99 _ = happyReduce_43

action_100 _ = happyReduce_44

action_101 _ = happyReduce_49

action_102 (36) = happyShift action_109
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (58) = happyShift action_108
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (36) = happyShift action_107
action_104 (11) = happyGoto action_106
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_6

action_106 (57) = happyShift action_115
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (56) = happyShift action_114
action_107 _ = happyReduce_15

action_108 (61) = happyShift action_113
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (57) = happyShift action_112
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_33

action_111 _ = happyReduce_38

action_112 (36) = happyShift action_118
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_2

action_114 (36) = happyShift action_117
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (36) = happyShift action_107
action_115 (11) = happyGoto action_116
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (63) = happyShift action_120
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_16

action_118 (63) = happyShift action_119
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (57) = happyShift action_123
action_119 _ = happyReduce_50

action_120 (57) = happyShift action_122
action_120 (10) = happyGoto action_121
action_120 _ = happyReduce_13

action_121 _ = happyReduce_12

action_122 (62) = happyShift action_125
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (62) = happyShift action_102
action_123 (26) = happyGoto action_124
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_51

action_125 (36) = happyShift action_107
action_125 (11) = happyGoto action_126
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (57) = happyShift action_127
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (36) = happyShift action_107
action_127 (11) = happyGoto action_128
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (63) = happyShift action_129
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (57) = happyShift action_122
action_129 (10) = happyGoto action_130
action_129 _ = happyReduce_13

action_130 _ = happyReduce_14

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn23  happy_var_3)
	(HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1, happy_var_2, happy_var_3)
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 11 5 happyReduction_2
happyReduction_2 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ((happy_var_5, happy_var_9)
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 4 6 happyReduction_3
happyReduction_3 ((HappyTerminal (LTInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Rectangle happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Outline happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (LTInt happy_var_1))
	 =  HappyAbsSyn7
		 ([(happy_var_1, happy_var_2)]
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 7 happyReduction_6
happyReduction_6 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal (LTInt happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_1, happy_var_2) : happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn8
		 (DirUp
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn8
		 (DirRight
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 (DirDown
	)

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn8
		 (DirLeft
	)

happyReduce_11 = happySpecReduce_0  9 happyReduction_11
happyReduction_11  =  HappyAbsSyn9
		 ([]
	)

happyReduce_12 = happyReduce 6 9 happyReduction_12
happyReduction_12 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((happy_var_2, happy_var_4) : happy_var_6
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_0  10 happyReduction_13
happyReduction_13  =  HappyAbsSyn10
		 ([]
	)

happyReduce_14 = happyReduce 7 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn10  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_3, happy_var_5) : happy_var_7
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyTerminal (LTInt happy_var_1))
	 =  HappyAbsSyn11
		 ((happy_var_1, happy_var_1)
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 (HappyTerminal (LTInt happy_var_3))
	_
	(HappyTerminal (LTInt happy_var_1))
	 =  HappyAbsSyn11
		 ((happy_var_1, happy_var_3)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 5 12 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ([(happy_var_2, happy_var_4)]
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 12 happyReduction_18
happyReduction_18 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_2, happy_var_4) : happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  13 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  14 happyReduction_21
happyReduction_21 (HappyTerminal (LTInt happy_var_3))
	_
	_
	 =  HappyAbsSyn14
		 (HP happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  14 happyReduction_22
happyReduction_22 (HappyAbsSyn18  happy_var_3)
	_
	_
	 =  HappyAbsSyn14
		 (Initiative happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 (HappyTerminal (LTInt happy_var_3))
	_
	_
	 =  HappyAbsSyn14
		 (Speed happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  14 happyReduction_24
happyReduction_24 (HappyTerminal (LTInt happy_var_3))
	_
	_
	 =  HappyAbsSyn14
		 (AC happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_3)
	_
	_
	 =  HappyAbsSyn14
		 (Attack happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_3)
	_
	_
	 =  HappyAbsSyn14
		 (FullAttack happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  14 happyReduction_27
happyReduction_27 (HappyAbsSyn21  happy_var_3)
	_
	_
	 =  HappyAbsSyn14
		 (Saves happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  15 happyReduction_28
happyReduction_28 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  15 happyReduction_29
happyReduction_29 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  16 happyReduction_30
happyReduction_30 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ((happy_var_1, happy_var_2, happy_var_3)
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  16 happyReduction_31
happyReduction_31 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ((happy_var_1,  0, happy_var_2)
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn17
		 (Melee
	)

happyReduce_33 = happyReduce 4 17 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyTerminal (LTInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Ranged happy_var_3
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_2  18 happyReduction_34
happyReduction_34 (HappyTerminal (LTInt happy_var_2))
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  18 happyReduction_35
happyReduction_35 (HappyTerminal (LTInt happy_var_2))
	_
	 =  HappyAbsSyn18
		 (- happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  19 happyReduction_36
happyReduction_36 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (DieRoll {dieAmount = fst happy_var_1, dieValue = snd happy_var_1, modifier = 0}
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  19 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (DieRoll {dieAmount = fst happy_var_1, dieValue = snd happy_var_1, modifier = happy_var_2}
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  20 happyReduction_38
happyReduction_38 (HappyTerminal (LTInt happy_var_3))
	_
	(HappyTerminal (LTInt happy_var_1))
	 =  HappyAbsSyn20
		 ((happy_var_1, happy_var_3)
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyTerminal (LTDie happy_var_1))
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  21 happyReduction_40
happyReduction_40 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  21 happyReduction_41
happyReduction_41 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  22 happyReduction_42
happyReduction_42 (HappyAbsSyn18  happy_var_3)
	_
	_
	 =  HappyAbsSyn22
		 (Fortitude happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  22 happyReduction_43
happyReduction_43 (HappyAbsSyn18  happy_var_3)
	_
	_
	 =  HappyAbsSyn22
		 (Reflex happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  22 happyReduction_44
happyReduction_44 (HappyAbsSyn18  happy_var_3)
	_
	_
	 =  HappyAbsSyn22
		 (Will happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happyReduce 5 23 happyReduction_45
happyReduction_45 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ([(happy_var_2, happy_var_4)]
	) `HappyStk` happyRest

happyReduce_46 = happyReduce 6 23 happyReduction_46
happyReduction_46 ((HappyAbsSyn23  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, happy_var_4) : happy_var_6
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_2  24 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  24 happyReduction_48
happyReduction_48 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 : happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 4 25 happyReduction_49
happyReduction_49 ((HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTCount happy_var_2)) `HappyStk`
	(HappyTerminal (LTVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ((happy_var_1, happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 5 26 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyTerminal (LTInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 ([(happy_var_2, happy_var_4)]
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 7 26 happyReduction_51
happyReduction_51 ((HappyAbsSyn26  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 ((happy_var_2, happy_var_4) : happy_var_7
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 64 64 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LTMap -> cont 27;
	LTLayout -> cont 28;
	LTRectangle -> cont 29;
	LTOutline -> cont 30;
	LTObstacles -> cont 31;
	LTDirUp -> cont 32;
	LTDirRight -> cont 33;
	LTDirDown -> cont 34;
	LTDirLeft -> cont 35;
	LTInt happy_dollar_dollar -> cont 36;
	LTDie happy_dollar_dollar -> cont 37;
	LTCount happy_dollar_dollar -> cont 38;
	LTVar happy_dollar_dollar -> cont 39;
	LTUnit -> cont 40;
	LTHP -> cont 41;
	LTInitiative -> cont 42;
	LTSpeed -> cont 43;
	LTAC -> cont 44;
	LTAttack -> cont 45;
	LTFullAttack -> cont 46;
	LTMelee -> cont 47;
	LTRanged -> cont 48;
	LTSaves -> cont 49;
	LTFortitude -> cont 50;
	LTReflex -> cont 51;
	LTWill -> cont 52;
	LTTeam -> cont 53;
	LTSym 'd' -> cont 54;
	LTSym '+' -> cont 55;
	LTSym '-' -> cont 56;
	LTSym ',' -> cont 57;
	LTSym ';' -> cont 58;
	LTSym ':' -> cont 59;
	LTSym '{' -> cont 60;
	LTSym '}' -> cont 61;
	LTSym '(' -> cont 62;
	LTSym ')' -> cont 63;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 64 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(LexerToken)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Direction = DirUp | DirRight | DirDown | DirLeft 
                 deriving Show

data Layout = Rectangle Int Int | Outline [(Int, Direction)] 
              deriving Show

type Coordinate = (Int, Int)

type Obstacle = (Coordinate, Coordinate) 

type Map = (Layout, [Obstacle])

data DieRoll = DieRoll { dieAmount :: Int,
                         dieValue :: Int, 
                         modifier :: Int } 
               deriving Show

data AttackRange = Melee | Ranged Int 
                   deriving Show

type AttackDesc = (AttackRange, Int, DieRoll)

data UnitSave = 
     Fortitude Int |
     Reflex Int    |
     Will Int 
     deriving Show

data UnitStat = 
	HP Int                   |
	Initiative Int           |
	Speed Int                |
	AC Int                   |
	Attack AttackDesc        |
	FullAttack [AttackDesc]  |
	Saves [UnitSave] 
	deriving Show

type StatBlock = (String, [UnitStat])

type Team = (String, [(String, Int, [Coordinate])])

type Game = (Map, [StatBlock], [Team])


parseError :: [LexerToken] -> a
parseError _ = error "Parse error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
