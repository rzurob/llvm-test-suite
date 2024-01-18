!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpEquivC578
!*
!*  DATE                       : Jul. 06, 2007
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
!*  C578 (R556) An equivalence-object shall not have the TARGET attribute.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpEquivC578

  TYPE :: DT(K)
    INTEGER, KIND :: K=4
    SEQUENCE
    INTEGER(K)    :: I
  END TYPE

  TYPE(DT), TARGET :: T

  EQUIVALENCE(T, I)

  END


