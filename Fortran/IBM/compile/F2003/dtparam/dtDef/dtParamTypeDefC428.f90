!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 30, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type definition
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
!*  C428 (R429) If ABSTRACT appears, the type shall be extensible.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE, ABSTRACT, PRIVATE :: DT1(K, L)
      INTEGER, KIND :: K=2
      INTEGER, LEN  :: L=1
      SEQUENCE
      INTEGER(K) :: I
    END TYPE

    TYPE, ABSTRACT, BIND(C)  :: DT2
    END TYPE

  END MODULE

  PROGRAM dtParamTypeDefC428

  END

