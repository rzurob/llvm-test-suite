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
!*  - MAXEXPONENT
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM       InitExpDefInqMAXEXPONENT
  IMPLICIT NONE
  INTEGER :: I, J, K


  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1

  INTEGER  :: TR4(128)  = (/(MAXEXPONENT(R4), I=1,128)/)
  INTEGER  :: TR8(128)  = (/(MAXEXPONENT(R8), I=1,128)/)
  INTEGER  :: TR6(128)  = (/(MAXEXPONENT(R6), I=1,128)/)


!???IEEE
!  IF ( ANY ( TR4  .NE. 127  )  )   ERROR STOP 11
!  IF ( ANY ( TR8  .NE. 1023 )  )   ERROR STOP 12
!  IF ( ANY ( TR6  .NE. 1023 )  )   ERROR STOP 13

! IBM extension
  IF ( ANY ( TR4  .NE. 128  )  )   ERROR STOP 11
  IF ( ANY ( TR8  .NE. 1024 )  )   ERROR STOP 12
  IF ( ANY ( TR6  .NE. 1024 )  )   ERROR STOP 13

  END



