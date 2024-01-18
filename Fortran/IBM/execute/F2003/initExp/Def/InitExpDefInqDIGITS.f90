!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefInqDIGITS.f
!*
!*  DATE                       : Apr. 03, 2006
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
!*  a reference to a specification inquiry
!*
!*  -  DIGITS
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM          InitExpDefInqDIGITS
  IMPLICIT NONE
  INTEGER :: I, J, K


  INTEGER(1),  PARAMETER :: I1(-2147483648:-2147483647, 2147483646:2147483647) = 0
  INTEGER(2),  PARAMETER :: I2 = 10
  INTEGER(4),  PARAMETER :: I4(-2147483648:-2147483647, 2147483646:2147483647) = 1
  INTEGER(8),  PARAMETER :: I8 = -1

  INTEGER  :: TI1  = DIGITS(I1)
  INTEGER  :: TI2  = DIGITS(I2)
  INTEGER  :: TI4  = DIGITS(I4)
  INTEGER  :: TI8  = DIGITS(I8)

  REAL(4),  PARAMETER :: R0(-2147483647:-2147483647, 6:5) = 0
  REAL(4),  PARAMETER :: R4(-2147483648:-2147483647, 2147483646:2147483647) = 0
  REAL(8),  PARAMETER :: R8 = 10
  REAL(16), PARAMETER :: R6(-2147483648:-2147483647, 2147483646:2147483647) = 1

  INTEGER  :: TR0  = DIGITS(R0)
  INTEGER  :: TR4  = DIGITS(R4)
  INTEGER  :: TR8  = DIGITS(R8)
  INTEGER  :: TR6  = DIGITS(R6)


  IF ( TI1    .NE. 7     )   STOP 11
  IF ( TI2    .NE. 15    )   STOP 12
  IF ( TI4    .NE. 31    )   STOP 13
  IF ( TI8    .NE. 63    )   STOP 14

  IF ( TR0    .NE. 24    )   STOP 20
  IF ( TR4    .NE. 24    )   STOP 21
  IF ( TR8    .NE. 53    )   STOP 22
  IF ( TR6    .NE. 106   )   STOP 23

  END



