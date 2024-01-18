!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpCommonC589
!*
!*  DATE                       : Jul. 13, 2007
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
!*   C589 (R558) If a common-block-object is of a derived type, it shall be a sequence type (4.5.1) or a
!*   type with the BIND attribute and it shall have no default initialization.
!*
!*  (339209)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpCommonC589
  IMPLICIT NONE

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER(K)    :: I(L)=K
  END TYPE

  TYPE(DT_I(2,7))  :: S, T
  COMMON //S
  COMMON /BLK/T

  END

