!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpCharOP.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 07 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER CharOPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Character operation
!*
!* (324975)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpCharOP
  IMPLICIT NONE

  INTEGER     :: I

  CHARACTER(128),  PARAMETER :: A(128)=                                                                               &
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


  CHARACTER(LEN=128, KIND=1)  :: C1(128)= [(REPEAT(A(I)(1:I),1) // REPEAT(A(I)(I+1:128), 1) , I=1,128)]
  CHARACTER(LEN=128, KIND=1)  :: C2(128)= [(REPEAT(A(I)(I:I),I) // REPEAT(A(I)(I:I), 128-I) , I=1,128)]
  CHARACTER(LEN=128, KIND=1)  :: C3(128)= [(REPEAT(A(I)(I:I),128) // REPEAT("", 128 ) , I=1,128)]
  CHARACTER(LEN=128, KIND=1)  :: C4(128)= [(REPEAT(A(I)(1:128),1) // REPEAT(" ", 128 ) , I=1,128)]


  IF ( ANY( C1 .NE. A ) ) ERROR STOP 11

  DO I = 1, 128
    IF ( C2(I) .NE. REPEAT(A(I)(I:I),I) // REPEAT(A(I)(I:I), 128-I) ) ERROR STOP 12
    IF ( C3(I) .NE. REPEAT(A(I)(I:I),128) ) ERROR STOP 13
  END DO


  IF ( ANY( C4 .NE. A ) ) ERROR STOP 14

  END


