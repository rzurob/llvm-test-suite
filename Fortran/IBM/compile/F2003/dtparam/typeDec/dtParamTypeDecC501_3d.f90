!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501_3d
!*
!*  DATE                       : Apr. 26, 2007
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
!*  C501 (R501) In a declaration-type-spec, every type-param-value that is
!*  not a colon or an asterisk shall be a specification-expr
!*
!*  -- "L" here is not host associated.
!*
!*  (336378)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER       :: I=K
  END TYPE

  TYPE(DT), SAVE :: T

  END MODULE

  PROGRAM dtParamTypeDecC501_3d
  USE M

  TYPE(DT(L=T%I)) :: T1
  CHARACTER(I)    :: C

  CONTAINS

  SUBROUTINE IntSub()
  TYPE(DT(4,     L=L)) :: T
  END SUBROUTINE

  END

