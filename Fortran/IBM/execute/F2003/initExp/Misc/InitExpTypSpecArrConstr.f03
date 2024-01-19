!*********************************************************************
!*  ===================================================================
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
!*  Type Spec in arr constructor
!*
!* (324676)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecArrConstr
  IMPLICIT NONE

  INTEGER :: I

  INTEGER(1), POINTER     :: I1
  INTEGER(2), POINTER     :: I2
  INTEGER(4), ALLOCATABLE :: I4
  INTEGER(8), ALLOCATABLE :: I8

  INTEGER                 :: II(128)= (/(-I, I=0, 127)/)

  INTEGER(1),   PARAMETER :: TI1(128)=(/ INTEGER(KIND=I1%KIND) :: (-I, I=0, 127)/)
  INTEGER(2),   PARAMETER :: TI2(128)=(/ INTEGER(KIND=I2%KIND) :: (-I, I=0, 127)/)
  INTEGER(4),   PARAMETER :: TI4(128)=(/ INTEGER(KIND=I4%KIND) :: (-I, I=0, 127)/)
  INTEGER(8),   PARAMETER :: TI8(128)=(/ INTEGER(KIND=I8%KIND) :: (-I, I=0, 127)/)

  LOGICAL(1),   PARAMETER :: TL1(128)=(/LOGICAL(KIND=I1%KIND) :: (.TRUE._8, I=127, 0, -1)/)
  LOGICAL(2),   PARAMETER :: TL2(128)=(/LOGICAL(KIND=I2%KIND) :: (.TRUE._8, I=127, 0, -1)/)
  LOGICAL(4),   PARAMETER :: TL4(128)=(/LOGICAL(KIND=I4%KIND) :: (.TRUE._8, I=127, 0, -1)/)
  LOGICAL(8),   PARAMETER :: TL8(128)=(/LOGICAL(KIND=I8%KIND) :: (.TRUE._8, I=127, 0, -1)/)

  IF ( KIND(TI1) .NE. 1      ) ERROR STOP 11
  IF ( KIND(TI2) .NE. 2      ) ERROR STOP 12
  IF ( KIND(TI4) .NE. 4      ) ERROR STOP 13
  IF ( KIND(TI8) .NE. 8      ) ERROR STOP 14

  IF ( ANY(TI1   .NE. II ) ) ERROR STOP 21
  IF ( ANY(TI2   .NE. II ) ) ERROR STOP 22
  IF ( ANY(TI4   .NE. II ) ) ERROR STOP 23
  IF ( ANY(TI8   .NE. II ) ) ERROR STOP 24

  IF ( KIND(TL1) .NE. 1      ) ERROR STOP 31
  IF ( KIND(TL2) .NE. 2      ) ERROR STOP 32
  IF ( KIND(TL4) .NE. 4      ) ERROR STOP 33
  IF ( KIND(TL8) .NE. 8      ) ERROR STOP 34

  IF ( ANY(TL1   .NEQV. .TRUE. ) ) ERROR STOP 41
  IF ( ANY(TL2   .NEQV. .TRUE. ) ) ERROR STOP 42
  IF ( ANY(TL4   .NEQV. .TRUE. ) ) ERROR STOP 43
  IF ( ANY(TL8   .NEQV. .TRUE. ) ) ERROR STOP 44

  END


