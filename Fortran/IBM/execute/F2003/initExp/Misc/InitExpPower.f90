!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sep. 06 2006
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
!*  **
!*  (324904)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpPower
  IMPLICIT NONE

  INTEGER    :: I

  REAL(4)    :: R4(128)=[(10.**2., I=1,128)]
  REAL(8)    :: R8(128)=[(10.**2,  I=1,128)]
  REAL(16)   :: R6(128)=[(10**2.,  I=1,128)]

  COMPLEX(4) :: Z4(128)=[((10.**2., -10**2.), I=1,128)]
  COMPLEX(8) :: Z8(128)=[((10**2., -10**2), I=1,128)]
  COMPLEX(16):: Z6(128)=[((10.**2, -10**2), I=1,128)]

  REAL       :: R = 10**2
  COMPLEX    :: Z = (10**2, -10**2)


  IF ( ANY(R4   .NE. R ) ) STOP 31
  IF ( ANY(R8   .NE. R ) ) STOP 32
  IF ( ANY(R6   .NE. R ) ) STOP 33

  IF ( ANY(Z4   .NE. Z ) ) STOP 41
  IF ( ANY(Z8   .NE. Z ) ) STOP 42
  IF ( ANY(Z6   .NE. Z ) ) STOP 43


  END



