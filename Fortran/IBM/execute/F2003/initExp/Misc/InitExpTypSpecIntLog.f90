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
!*  intrinsic-type-spec : integer and logical
!*
!*  (324642)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecIntLog
  IMPLICIT NONE

  INTEGER :: I

  INTEGER,                       PARAMETER :: R(128) = (/(-I, I=1, 128)/)

  INTEGER(KIND=1_8),             PARAMETER :: I1(128)=(/(-I, I=1, 128)/)
  INTEGER(KIND=I1%KIND+1),       PARAMETER :: I2(128)=(/(-I, I=1, 128)/)
  INTEGER(KIND=2*I2%KIND),       PARAMETER :: I4(128)=(/(-I, I=1, 128)/)
  INTEGER(KIND=I4%KIND+I4%KIND), PARAMETER :: I8(128)=(/(-I, I=1, 128)/)

  LOGICAL(KIND=I1%KIND),         PARAMETER :: L1(128)=(/(.TRUE._8, I=127, 0, -1)/)
  LOGICAL(KIND=L1%KIND+1),       PARAMETER :: L2(128)=(/(.TRUE._4, I=127, 0, -1)/)
  LOGICAL(KIND=2*L2%KIND),       PARAMETER :: L4(128)=(/(.TRUE._2, I=127, 0, -1)/)
  LOGICAL(KIND=L4%KIND+L4%KIND), PARAMETER :: L8(128)=(/(.TRUE._1, I=127, 0, -1)/)

  IF ( KIND(I1) .NE. 1      ) ERROR STOP 11
  IF ( KIND(I2) .NE. 2      ) ERROR STOP 12
  IF ( KIND(I4) .NE. 4      ) ERROR STOP 13
  IF ( KIND(I8) .NE. 8      ) ERROR STOP 14

  IF ( ANY(I1   .NE. R ) ) ERROR STOP 21
  IF ( ANY(I2   .NE. R ) ) ERROR STOP 22
  IF ( ANY(I4   .NE. R ) ) ERROR STOP 23
  IF ( ANY(I8   .NE. R ) ) ERROR STOP 24

  IF ( KIND(L1) .NE. 1      ) ERROR STOP 31
  IF ( KIND(L2) .NE. 2      ) ERROR STOP 32
  IF ( KIND(L4) .NE. 4      ) ERROR STOP 33
  IF ( KIND(L8) .NE. 8      ) ERROR STOP 34

  IF ( ANY(L1   .NEQV. .TRUE. ) ) ERROR STOP 41
  IF ( ANY(L2   .NEQV. .TRUE. ) ) ERROR STOP 42
  IF ( ANY(L4   .NEQV. .TRUE. ) ) ERROR STOP 43
  IF ( ANY(L8   .NEQV. .TRUE. ) ) ERROR STOP 44

  END


