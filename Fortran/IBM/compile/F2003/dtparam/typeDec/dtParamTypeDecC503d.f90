!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC503d
!*
!*  DATE                       : May. 07, 2007
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
!* C503 (R502) The TYPE(derived-type-spec) shall not specify an abstract type
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC503d

  TYPE, ABSTRACT :: DT(K)
    INTEGER, KIND :: K=4
  END TYPE

  TYPE(DT(K=4))  :: T

  TYPE, ABSTRACT :: DT1(L)
    INTEGER, LEN  :: L=1
  END TYPE

  TYPE(DT1(L=1))  :: T1

  CLASS(DT1(L=1)), POINTER  :: T2 ! this is ok

  SELECT TYPE (T2)
  TYPE IS (DT1(L=*))
  END SELECT
  END

