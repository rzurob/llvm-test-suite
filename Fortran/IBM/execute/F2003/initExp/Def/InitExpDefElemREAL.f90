!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 14, 2006
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
!*  -  REAL
!*  (319256)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemREAL
  IMPLICIT NONE
  INTEGER :: I, J


  REAL(KIND(RESHAPE(REAL(A=(/(z"FF", J=1,16)/), KIND=4),(/4,4/)))) ::     &
   TI4(4,4)=RESHAPE(REAL(A=(/(z"FF", J=1,16)/), KIND=4),(/4,4/))

  REAL(KIND(RESHAPE(REAL(A=(/(z"FFFF", J=1,16)/), KIND=8),(/4,4/)))) ::     &
   TI8(4,4)=RESHAPE(REAL(A=(/(z"FFFF", J=1,16)/), KIND=8),(/4,4/))

  REAL(KIND(RESHAPE(REAL(A=(/(z"FFFFFFFF", J=1,16)/), KIND=16),(/4,4/)))) ::    &
   TI6(4,4)=RESHAPE(REAL(A=(/(z"FFFFFFFF", J=1,16)/), KIND=16),(/4,4/))

  REAL(KIND(RESHAPE(REAL(A=(/REAL(z"0FFFFFFF")/), KIND=4),(/1,1/)))) ::    &
    TX(1,1)=RESHAPE(REAL(A=(/REAL(z"0FFFFFFF")/), KIND=4),(/1,1/))

  CHARACTER(32) :: C
  IF ( KIND(TI4)  .NE.    4 )  STOP 11
  IF ( KIND(TI8)  .NE.    8 )  STOP 12
  IF ( KIND(TI6)  .NE.   16 )  STOP 13
  IF ( KIND(TX)   .NE.    4 )  STOP 14

  WRITE(C, FMT="(Z32.32)") TI4(1,1)
  IF ( C  .NE.   REPEAT("0", 30) // "FF" )  STOP 21
  WRITE(C, FMT="(Z32.32)") TI8(1,1)
  IF ( C  .NE.   REPEAT("0", 28) // "FFFF" )  STOP 22
  WRITE(C, FMT="(Z32.32)") TI6(1,1)
  IF ( C  .NE.   REPEAT("0", 24) // "FFFFFFFF" )  STOP 23

  WRITE(C, FMT="(Z8.8)") TX
  IF ( C  .NE.  "0FFFFFFF" )  STOP 24

  END


