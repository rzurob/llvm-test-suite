!*********************************************************************
!*  ===================================================================
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
!*  -- An object designator with a base object that is a dummy argument that
!*     has neither the OPTIONAL nor the INTENT (OUT) attribute
!*
!*  (336581)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC501_1d

  TYPE :: DT(Kind,Len)
    INTEGER, KIND :: Kind=4
    INTEGER, LEN  :: Len=1
  END TYPE

  CONTAINS

  SUBROUTINE IntSub(T)
  TYPE(DT), OPTIONAL :: T
  TYPE(DT(Len=T%Len))  :: T1
  END SUBROUTINE


  SUBROUTINE IntSub1(T)
  TYPE(DT), INTENT(OUT) :: T
  TYPE(DT(Len=T%Kind))     :: T1
  END SUBROUTINE


  END

