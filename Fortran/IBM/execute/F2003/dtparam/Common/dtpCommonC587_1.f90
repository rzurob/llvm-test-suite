!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpCommonC587_1
!*
!*  DATE                       : Jul. 05, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- The common statement
!*
!*  C587 (R558) Only one appearance of a given variable-name or proc-pointer-name is permitted in all
!*  common-block-object-lists within a scoping unit.
!*  -- Test procedure pointers
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_C(K,L)
    INTEGER, KIND :: K!=4
    INTEGER, LEN  :: L!=4
    SEQUENCE
    CHARACTER(L)  :: C(L)!=CHAR(48+K)
  END TYPE


  CONTAINS

  FUNCTION F()
  TYPE(DT_C(4,128))  :: F, C, C1
  COMMON /BLK/C, C1
    F = C
  END FUNCTION

  END MODULE


  BLOCK DATA
  USE M

  TYPE(DT_C(4,128))  :: C, C1
  DATA C /DT_C(4,128)([(CHAR(I),I=0,127)])/
  !PROCEDURE(F), POINTER :: ProcPtr  <- not allowed
  COMMON /BLK/C , C1 !, ProcPtr

  END BLOCK DATA


  PROGRAM dtpCommonC587_1
  USE M

  TYPE(DT_C(4,128))  :: C, C1
  PROCEDURE(F), POINTER :: ProcPtr
  COMMON /BLK/C, C1
  COMMON ProcPtr

  IF ( SIZE(C%C)    .NE. 128 )                   STOP 11
  IF ( C%C%LEN      .NE. 128 )                   STOP 12
  IF ( ANY(C%C      .NE. [(CHAR(I),I=0,127)] ) ) STOP 13

  CALL ExtSub()

  C1 = ProcPtr()

  IF ( ANY(C%C      .NE. [(CHAR(I),I=127, 0, -1)] ) ) STOP 14
  IF ( ANY(C1%C     .NE. [(CHAR(I),I=127, 0, -1)] ) ) STOP 15

  END

  SUBROUTINE ExtSub()
  USE M
  TYPE(DT_C(4,128))  :: C, C1
  PROCEDURE(F), POINTER :: ProcPtr
  COMMON /BLK/C, C1
  COMMON ProcPtr
  INTEGER I

  C%C = [(CHAR(I), I=127,0,-1)]
  ProcPtr => F

  END SUBROUTINE
