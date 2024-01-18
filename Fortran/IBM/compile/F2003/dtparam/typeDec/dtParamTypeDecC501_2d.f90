!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501_2d
!*
!*  DATE                       : Apr. 24, 2007
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
!*  -- An object designator with a base object that is in a common block
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC501_2d

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER       :: I=K
  END TYPE

  TYPE(DT(K=4_1, L=1_8)) :: T(1)
  COMMON /A/T

  T = [DT(K=4_1, L=1_8)()]

  CALL IntSub(  )

  CONTAINS

  SUBROUTINE IntSub()

  TYPE(DT(4,     L=T(1)%I))  :: T2(1)  ! This is fine

  INTEGER              ::I
  TYPE(DT(4,     L=I)) :: T4(1)

  END SUBROUTINE

  END

  END

