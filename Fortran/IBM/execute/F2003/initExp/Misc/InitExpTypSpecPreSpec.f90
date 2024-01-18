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
!*  intrinsic-type-spec in pre-sprec: integer and logical
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  INTEGER(KIND=1_8) FUNCTION I1()
    I1 = 1
  END FUNCTION

  INTEGER(KIND=KIND([1_1])+1) FUNCTION I2()
    I2 = 2
  END FUNCTION

  INTEGER(KIND=KIND([1_2])*2) FUNCTION I4()
    I4 = 4
  END FUNCTION

  INTEGER(KIND=KIND([1_8])*1) FUNCTION I8()
    I8 = 8
  END FUNCTION


  LOGICAL(KIND=KIND([1_1])) FUNCTION L1()
    L1 = .TRUE.
  END FUNCTION

  LOGICAL(KIND=KIND([1_2])) FUNCTION L2()
    L2 = .TRUE.
  END FUNCTION

  LOGICAL(KIND=KIND([1_4])) FUNCTION L4()
    L4 = .TRUE.
  END FUNCTION

  LOGICAL(KIND=KIND([1_8])) FUNCTION L8()
    L8 = .TRUE.
  END FUNCTION

  END MODULE


  PROGRAM InitExpTypSpecPreSpecr
  USE M
  IMPLICIT NONE

  INTEGER :: I


  PROCEDURE(INTEGER(KIND=1_8)),             POINTER :: I1PreSpecr
  PROCEDURE(INTEGER(KIND=KIND([1_1])+1)),   POINTER :: I2PreSpecr
  PROCEDURE(INTEGER(KIND=KIND([1_2])*2)),   POINTER :: I4PreSpecr
  PROCEDURE(INTEGER(KIND=KIND([1_8])*1)),   POINTER :: I8PreSpecr

  PROCEDURE(LOGICAL(KIND=KIND([1_1]))),     POINTER :: L1PreSpecr
  PROCEDURE(LOGICAL(KIND=KIND([1_2]))),     POINTER :: L2PreSpecr
  PROCEDURE(LOGICAL(KIND=KIND([1_4]))),     POINTER :: L4PreSpecr
  PROCEDURE(LOGICAL(KIND=KIND([1_8]))),     POINTER :: L8PreSpecr

  I1PreSpecr => I1
  I2PreSpecr => I2
  I4PreSpecr => I4
  I8PreSpecr => I8

  L1PreSpecr => L1
  L2PreSpecr => L2
  L4PreSpecr => L4
  L8PreSpecr => L8


  IF ( I1PreSpecr() .NE. 1      ) STOP 11
  IF ( I2PreSpecr() .NE. 2      ) STOP 12
  IF ( I4PreSpecr() .NE. 4      ) STOP 13
  IF ( I8PreSpecr() .NE. 8      ) STOP 14

  IF ( L1PreSpecr() .NEQV. .TRUE. ) STOP 21
  IF ( L2PreSpecr() .NEQV. .TRUE. ) STOP 22
  IF ( L4PreSpecr() .NEQV. .TRUE. ) STOP 23
  IF ( L8PreSpecr() .NEQV. .TRUE. ) STOP 24

  END


