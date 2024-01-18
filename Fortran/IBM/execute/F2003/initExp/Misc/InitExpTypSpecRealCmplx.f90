!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 29, 2006
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
!*  intrinsic-type-spec : REAL and COMPLEX
!*
!*  (324642)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecRealCmplx
  IMPLICIT NONE

  INTEGER :: I

  REAL(KIND=4_8),             PARAMETER :: R4(128)=(/(-I, I=1, 128)/)
  REAL(KIND=2*R4%KIND),       PARAMETER :: R8(128)=(/(-I, I=1, 128)/)
  REAL(KIND=R8%KIND+R8%KIND), PARAMETER :: R6(128)=(/(-I, I=1, 128)/)

  COMPLEX(KIND=SIZE((/(-I,I=1,4)/))), PARAMETER :: Z4(128)= R4
  COMPLEX(KIND=LBOUND(Z4,1)+7),       PARAMETER :: Z8(128)= R8
  COMPLEX(KIND=2*R8%KIND),            PARAMETER :: Z6(128)= R6

  IF ( KIND(R4) .NE. 4      ) STOP 12
  IF ( KIND(R8) .NE. 8      ) STOP 13
  IF ( KIND(R6) .NE. 16     ) STOP 14

  IF ( ANY(R4   .NE. (/(-I, I=1, 128)/) ) ) STOP 22
  IF ( ANY(R8   .NE. (/(-I, I=1, 128)/) ) ) STOP 23
  IF ( ANY(R6   .NE. (/(-I, I=1, 128)/) ) ) STOP 24

  IF ( KIND(Z4) .NE. 4      ) STOP 32
  IF ( KIND(Z8) .NE. 8      ) STOP 33
  IF ( KIND(Z6) .NE. 16     ) STOP 34

  IF ( ANY(Z4   .NE. R4 ) ) STOP 42
  IF ( ANY(Z8   .NE. R8 ) ) STOP 43
  IF ( ANY(Z6   .NE. R6 ) ) STOP 44

  END


