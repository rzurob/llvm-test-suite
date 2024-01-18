!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecArrConstr1.f
!*
!*  DATE                       : Aug. 30, 2006
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
!*  type-spec in array constructor
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecArrConstr1
  IMPLICIT NONE

  INTEGER :: I
  REAL(4) :: R4
  REAL(8) :: R8
  REAL(16):: R6

  REAL(KIND=KIND(R4)),   PARAMETER :: TR4(128)=[REAL(KIND=KIND(R4)) :: (-I, I=1, 128)]
  REAL(KIND=KIND(R8)),   PARAMETER :: TR8(128)=[REAL(KIND=KIND(R8)) :: (-I, I=1, 128)]
  REAL(KIND=KIND(R6)),   PARAMETER :: TR6(128)=[REAL(KIND=KIND(R6)) :: (-I, I=1, 128)]

  COMPLEX(KIND=KIND(R4)),PARAMETER :: TZ4(128)= [COMPLEX(KIND=KIND(R4)) :: (-I, I=1, 128)]
  COMPLEX(KIND=KIND(R8)),PARAMETER :: TZ8(128)= [COMPLEX(KIND=KIND(R8)) :: (-I, I=1, 128)]
  COMPLEX(KIND=KIND(R6)),PARAMETER :: TZ6(128)= [COMPLEX(KIND=KIND(R6)) :: (-I, I=1, 128)]

  IF ( KIND(TR4) .NE. 4      ) STOP 12
  IF ( KIND(TR8) .NE. 8      ) STOP 13
  IF ( KIND(TR6) .NE. 16     ) STOP 14

  IF ( ANY(TR4   .NE. (/(-I, I=1, 128)/) ) ) STOP 22
  IF ( ANY(TR8   .NE. (/(-I, I=1, 128)/) ) ) STOP 23
  IF ( ANY(TR6   .NE. (/(-I, I=1, 128)/) ) ) STOP 24

  IF ( KIND(TZ4) .NE. 4      ) STOP 32
  IF ( KIND(TZ8) .NE. 8      ) STOP 33
  IF ( KIND(TZ6) .NE. 16     ) STOP 34

  IF ( ANY(TZ4   .NE. TR4 ) ) STOP 42
  IF ( ANY(TZ8   .NE. TR8 ) ) STOP 43
  IF ( ANY(TZ6   .NE. TR6 ) ) STOP 44

  END


