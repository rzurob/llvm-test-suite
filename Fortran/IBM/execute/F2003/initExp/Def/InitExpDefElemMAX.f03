!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an elemental intrinsic
!*
!*  -  MAX
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemMAX
  IMPLICIT NONE
  INTEGER :: I, J


  CHARACTER(128), PARAMETER  :: C =                                                                     &
  CHAR(0)//CHAR(1)//CHAR(2)//CHAR(3)//CHAR(4)//CHAR(5)//CHAR(6)//CHAR(7)//CHAR(8)//CHAR(9)//            &
  CHAR(10)//CHAR(11)//CHAR(12)//CHAR(13)//CHAR(14)//CHAR(15)//CHAR(16)//CHAR(17)//CHAR(18)//CHAR(19)//  &
  CHAR(20)//CHAR(21)//CHAR(22)//CHAR(23)//CHAR(24)//CHAR(25)//CHAR(26)//CHAR(27)//CHAR(28)//CHAR(29)//  &
  CHAR(30)//CHAR(31)//CHAR(32)//CHAR(33)//CHAR(34)//CHAR(35)//CHAR(36)//CHAR(37)//CHAR(38)//CHAR(39)//  &
  CHAR(40)//CHAR(41)//CHAR(42)//CHAR(43)//CHAR(44)//CHAR(45)//CHAR(46)//CHAR(47)//CHAR(48)//CHAR(49)//  &
  CHAR(50)//CHAR(51)//CHAR(52)//CHAR(53)//CHAR(54)//CHAR(55)//CHAR(56)//CHAR(57)//CHAR(58)//CHAR(59)//  &
  CHAR(60)//CHAR(61)//CHAR(62)//CHAR(63)//CHAR(64)//CHAR(65)//CHAR(66)//CHAR(67)//CHAR(68)//CHAR(69)//  &
  CHAR(70)//CHAR(71)//CHAR(72)//CHAR(73)//CHAR(74)//CHAR(75)//CHAR(76)//CHAR(77)//CHAR(78)//CHAR(79)//  &
  CHAR(80)//CHAR(81)//CHAR(82)//CHAR(83)//CHAR(84)//CHAR(85)//CHAR(86)//CHAR(87)//CHAR(18)//CHAR(89)//  &
  CHAR(90)//CHAR(91)//CHAR(92)//CHAR(93)//CHAR(94)//CHAR(95)//CHAR(96)//CHAR(97)//CHAR(98)//CHAR(99)//  &
  CHAR(100)//CHAR(101)//CHAR(102)//CHAR(103)//CHAR(104)//CHAR(105)//CHAR(106)//CHAR(107)//CHAR(108)//CHAR(109)//  &
  CHAR(110)//CHAR(111)//CHAR(112)//CHAR(113)//CHAR(114)//CHAR(115)//CHAR(116)//CHAR(117)//CHAR(118)//CHAR(119)//  &
  CHAR(120)//CHAR(121)//CHAR(122)//CHAR(123)//CHAR(124)//CHAR(125)//CHAR(126)//CHAR(127)



  TYPE :: DT0
    INTEGER(1)  :: I1(128)=(/(INT(I, 1), I=0,127)/)
    INTEGER(2)  :: I2(128)=(/(INT(I, 2), I=0,127)/)
    INTEGER(4)  :: I4(128)=(/(INT(I, 4), I=0,127)/)
    INTEGER(8)  :: I8(128)=(/(INT(I, 8), I=0,127)/)
    REAL(4)     :: R4(128) =(/(I, I=0,127)/)
    REAL(8)     :: R8(128) =(/(I, I=0,127)/)
    REAL(16)    :: R6(128) =(/(I, I=0,127)/)
    CHARACTER(128) :: Str(128) = C
  END TYPE

  TYPE :: DT
    INTEGER(1)  :: I1(16)
    INTEGER(2)  :: I2(16)
    INTEGER(4)  :: I4(16)
    INTEGER(8)  :: I8(16)
    REAL(4)     :: R4(16)
    REAL(8)     :: R8(16)
    REAL(16)    :: R6(16)
    CHARACTER(128) :: Str(16)
  END TYPE

  TYPE (DT0), PARAMETER  :: T=DT0()

  TYPE (DT) :: T1=DT(                                                                      &
                        I1 =MAX(T%I1(2:17), (/(INT(I, 1), I=0,15)/), T%I1(3:18)),          &
                        I2 =MAX(T%I2(2:17), (/(INT(I, 2), I=0,15)/), T%I2(3:18)),          &
                        I4 =MAX(T%I4(2:17), (/(INT(I, 4), I=0,15)/), T%I4(3:18)),          &
                        I8 =MAX(T%I8(2:17), (/(INT(I, 8), I=0,15)/), T%I8(3:18)),          &
                        R4 =MAX(T%R4(2:17), (/(REAL(I, 4),I=0,15)/), T%R4(3:18)),          &
                        R8 =MAX(T%R8(2:17), (/(REAL(I, 8),I=0,15)/), T%R8(3:18)),          &
                        R6 =MAX(T%R6(2:17), (/(REAL(I,16),I=0,15)/), T%R6(3:18)),          &
                        Str=MAX(T%Str(1:16), T%Str(::8)(1:127), T%Str(1:16)(1:), T%Str(::8)(:))  &
                      )


  INTEGER :: Length = LEN(MAX(T%Str(:)(1:0),  T%Str(:128)(1:127), T%Str(:)(125:127), T%Str(:)))

  IF ( ANY (T1%I1   .NE. (/(I, I=2,17)/)) ) ERROR STOP 11
  IF ( ANY (T1%I2   .NE. (/(I, I=2,17)/)) ) ERROR STOP 12
  IF ( ANY (T1%I4   .NE. (/(I, I=2,17)/)) ) ERROR STOP 13
  IF ( ANY (T1%I8   .NE. (/(I, I=2,17)/)) ) ERROR STOP 14
  IF ( ANY (T1%R4   .NE. (/(I, I=2,17)/)) ) ERROR STOP 15
  IF ( ANY (T1%R8   .NE. (/(I, I=2,17)/)) ) ERROR STOP 16
  IF ( ANY (T1%R6   .NE. (/(I, I=2,17)/)) ) ERROR STOP 17

  IF ( ANY(T1%Str   .NE. C )  ) ERROR STOP 18
  IF ( Length       .NE. 128 )  ERROR STOP 19

  END

