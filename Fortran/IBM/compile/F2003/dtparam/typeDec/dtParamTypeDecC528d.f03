!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 11, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
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
!*  C528 (R501) If the VALUE attribute is specified, the length type parameter values
!*  shall be omitted or specified by initialization expressions.
!*
!*  (336717)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC528d

  TYPE :: DT(K, L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    INTEGER :: I=K
  END TYPE
  INTEGER :: I=4

  CONTAINS
  SUBROUTINE IntSub(T1, T2)

  TYPE(DT(1, I)), VALUE  :: T1
  TYPE(DT(1, *)), VALUE  :: T2

  END SUBROUTINE

  END

