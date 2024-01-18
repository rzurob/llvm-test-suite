!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpEquivC579
!*
!*  DATE                       : Jul. 09, 2007
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
!*  -- The equivalence statement
!*
!*  C579 (R556) Each subscript or substring range expression in an equivalence-object
!*  shall be an integer initialization expression
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpEquivC579

  INTEGER :: J

  CONTAINS

  SUBROUTINE Sub(I)

  TYPE :: DT(K, L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER(K)    :: I(L)
  END TYPE

  TYPE(DT(4,1)) :: T(1), S(1), R

  EQUIVALENCE(T(I), S)
  EQUIVALENCE(S(J), R)

  END SUBROUTINE
  END

