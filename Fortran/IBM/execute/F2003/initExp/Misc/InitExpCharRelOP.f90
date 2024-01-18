!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpCharRelOP.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 07 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Charber 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Char relational operation
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpCharRelOP
  IMPLICIT NONE

  INTEGER     :: I


  CHARACTER(LEN=128),  PARAMETER :: C(128)=                                                                           &
                 CHAR(0) //CHAR(1) //CHAR(2) //CHAR(3) //CHAR(4) //CHAR(5) //CHAR(6) //CHAR(7) //CHAR(8) //CHAR(9)    &
            //   CHAR(10)//CHAR(11)//CHAR(12)//CHAR(13)//CHAR(14)//CHAR(15)//CHAR(16)//CHAR(17)//CHAR(18)//CHAR(19)   &
            //   CHAR(20)//CHAR(21)//CHAR(22)//CHAR(23)//CHAR(24)//CHAR(25)//CHAR(26)//CHAR(27)//CHAR(28)//CHAR(29)   &
            //   CHAR(30)//CHAR(31)//CHAR(32)//CHAR(33)//CHAR(34)//CHAR(35)//CHAR(36)//CHAR(37)//CHAR(38)//CHAR(39)   &
            //   CHAR(40)//CHAR(41)//CHAR(42)//CHAR(43)//CHAR(44)//CHAR(45)//CHAR(46)//CHAR(47)//CHAR(48)//CHAR(49)   &
            //   CHAR(50)//CHAR(51)//CHAR(52)//CHAR(53)//CHAR(54)//CHAR(55)//CHAR(56)//CHAR(57)//CHAR(58)//CHAR(59)   &
            //   CHAR(60)//CHAR(61)//CHAR(62)//CHAR(63)//CHAR(64)//CHAR(65)//CHAR(66)//CHAR(67)//CHAR(68)//CHAR(69)   &
            //   CHAR(70)//CHAR(71)//CHAR(72)//CHAR(73)//CHAR(74)//CHAR(75)//CHAR(76)//CHAR(77)//CHAR(78)//CHAR(79)   &
            //   CHAR(80)//CHAR(81)//CHAR(82)//CHAR(83)//CHAR(84)//CHAR(85)//CHAR(86)//CHAR(87)//CHAR(88)//CHAR(89)   &
            //   CHAR(90)//CHAR(91)//CHAR(92)//CHAR(93)//CHAR(94)//CHAR(95)//CHAR(96)//CHAR(97)//CHAR(98)//CHAR(99)   &
            //   CHAR(100)//CHAR(101)//CHAR(102)//CHAR(103)//CHAR(104)//CHAR(105)//CHAR(106)//CHAR(107)//CHAR(108)//CHAR(109)   &
            //   CHAR(110)//CHAR(111)//CHAR(112)//CHAR(113)//CHAR(114)//CHAR(115)//CHAR(116)//CHAR(117)//CHAR(118)//CHAR(119)   &
            //   CHAR(120)//CHAR(121)//CHAR(122)//CHAR(123)//CHAR(124)//CHAR(125)//CHAR(126)//CHAR(127)



  LOGICAL :: L1(128) = C           <  C(128:1:-1)  .EQV. C(128:1:-1)       <  C
  LOGICAL :: L2(128) = C           <  C // " "     .EQV. C // " "          <  C
  LOGICAL :: L3(128) = C(:)(1:127) <  C            .EQV. C(:)(1:127)// " " <  C

  LOGICAL :: L4(128) = C           <= C(128:1:-1)  .EQV. C(128:1:-1)       <= C
  LOGICAL :: L5(128) = C           <= C // " "     .EQV. C // " "          <= C
  LOGICAL :: L6(128) = C(:)(1:127) <= C            .EQV. C(:)(1:127)// " " <= C

  LOGICAL :: L7(128) = C           == C(128:1:-1)  .EQV. C(128:1:-1)       == C
  LOGICAL :: L8(128) = C           == C // " "     .EQV. C // " "          == C
  LOGICAL :: L9(128) = C(:)(1:127) == C            .EQV. C(:)(1:127)// " " == C

  LOGICAL :: L11(128) = C           /= C(128:1:-1)  .EQV. C(128:1:-1)       /= C
  LOGICAL :: L12(128) = C           /= C // " "     .EQV. C // " "          /= C
  LOGICAL :: L13(128) = C(:)(1:127) /= C            .EQV. C(:)(1:127)// " " /= C

  LOGICAL :: L14(128) = C           >  C(128:1:-1)  .EQV. C(128:1:-1)       >  C
  LOGICAL :: L15(128) = C           >  C // " "     .EQV. C // " "          >  C
  LOGICAL :: L16(128) = C(:)(1:127) >  C            .EQV. C(:)(1:127)// " " >  C

  LOGICAL :: L17(128) = C           >= C(128:1:-1)  .EQV. C(128:1:-1)       >= C
  LOGICAL :: L18(128) = C           >= C // " "     .EQV. C // " "          >= C
  LOGICAL :: L19(128) = C(:)(1:127) >= C            .EQV. C(:)(1:127)// " " >= C



  IF ( ANY( .NOT. L1  ) ) STOP 11
  IF ( ANY( .NOT. L2  ) ) STOP 12
  IF ( ANY( .NOT. L3  ) ) STOP 13
  IF ( ANY( .NOT. L4  ) ) STOP 14
  IF ( ANY( .NOT. L5  ) ) STOP 15
  IF ( ANY( .NOT. L6  ) ) STOP 16
  IF ( ANY( .NOT. L7  ) ) STOP 17
  IF ( ANY( .NOT. L8  ) ) STOP 18
  IF ( ANY( .NOT. L9  ) ) STOP 19

  IF ( ANY( .NOT. L11  ) ) STOP 111
  IF ( ANY( .NOT. L12  ) ) STOP 112
  IF ( ANY( .NOT. L13  ) ) STOP 113
  IF ( ANY( .NOT. L14  ) ) STOP 114
  IF ( ANY( .NOT. L15  ) ) STOP 115
  IF ( ANY( .NOT. L16  ) ) STOP 116
  IF ( ANY( .NOT. L17  ) ) STOP 117
  IF ( ANY( .NOT. L18  ) ) STOP 118
  IF ( ANY( .NOT. L19  ) ) STOP 119




  END



