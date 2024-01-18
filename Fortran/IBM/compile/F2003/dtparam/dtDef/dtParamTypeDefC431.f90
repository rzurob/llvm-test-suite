!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefC431
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
!*  C431 (R433) If END TYPE is followed by a type-name, the type-name shall be
!*  the same as that in the corresponding derived-type-stmt.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefC431

    TYPE :: DT0(K,L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE :: DT1(K,L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE DT1

    TYPE, EXTENDS(DT0) :: DT3(K1,L1)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: L1
    END TYPE dt3

    TYPE :: DT2(K,L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE DT1

  END

