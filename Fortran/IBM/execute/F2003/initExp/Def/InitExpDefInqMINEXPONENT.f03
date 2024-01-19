!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 04, 2006
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
!*  - MINEXPONENT
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM       InitExpDefInqMINEXPONENT
  IMPLICIT NONE
  INTEGER :: I, J, K


  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1

  INTEGER  :: TR4(16, 16)  = RESHAPE((/(MINEXPONENT(R4), I=1,256)/), (/16,16/))
  INTEGER  :: TR8(16, 16)  = RESHAPE((/(MINEXPONENT(R8), I=1,256)/), (/16,16/))
  INTEGER  :: TR6(16, 16)  = RESHAPE((/(MINEXPONENT(R6), I=1,256)/), (/16,16/))

!?? IEEE
!  IF ( ANY ( TR4  .NE. -126  )  )   ERROR STOP 21
! IF ( ANY ( TR8  .NE. -1023 )  )   ERROR STOP 22
! IF ( ANY ( TR6  .NE. -1023 )  )   ERROR STOP 23

!?? IBM Eextension
  IF ( ANY ( TR4  .NE. -125  )  )   ERROR STOP 21
  IF ( ANY ( TR8  .NE. -1021 )  )   ERROR STOP 22
  IF ( ANY ( TR6  .NE. -968 )  )    ERROR STOP 23

  END



