!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpEquivC577
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
!*  C577 (R556) An equivalence-object shall not be a designator that has more than one part-ref
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpEquivC577

  TYPE :: DT(K)
    INTEGER, KIND :: K=4
    SEQUENCE
    INTEGER(K)    :: I
  END TYPE

  TYPE(DT) T

  EQUIVALENCE(T%I, I)

  END


