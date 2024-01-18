!*********************************************************************
!*  ===================================================================
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
!*  -  EPSILON
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM          InitExpDefInqEPSILON
  IMPLICIT NONE
  INTEGER :: I, J, K


  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1

  REAL(4)  :: TR4  = EPSILON(R4)
  REAL(8)  :: TR8  = EPSILON(R8)
  REAL(16) :: TR6  = EPSILON(R6)

  REAL(4),  PARAMETER   :: Epsilon4 = 2._4 **(-DIGITS(R4)+1)
  REAL(8),  PARAMETER   :: Epsilon8 = 2._8 **(-DIGITS(R8)+1)
  REAL(16), PARAMETER   :: Epsilon6 = 2._16**(-DIGITS(R6)+1)


  IF ( TR4    .NE. Epsilon4     )   ERROR STOP 11
  IF ( TR8    .NE. Epsilon8     )   ERROR STOP 18
  IF ( TR6    .NE. Epsilon6     )   ERROR STOP 16

  END



