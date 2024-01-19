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
!*  C522 (R501) The initialization shall appear if the statement contains
!*  a PARAMETER attribute
!*
!*  (336703)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC522d

  TYPE :: DT5(K, L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    INTEGER :: I=K
  END TYPE
  INTEGER :: I=4

  CONTAINS
  SUBROUTINE IntSub()

  TYPE(DT5(4, 4)), PARAMETER :: T=DT5(4,L=I)()
  TYPE(DT5(4, 4)), PARAMETER :: S=DT5(I)

  END SUBROUTINE


  END

  END

