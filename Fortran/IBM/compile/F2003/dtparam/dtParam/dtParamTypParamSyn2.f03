!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 26, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type parameters
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
!*  Type param def stmt - Syntax
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  TYPE :: DT1(K)
    INTEGER(=1)  KIND :: K=0
  END TYPE

  TYPE :: DT2(L)
    INTEGER(), LEN :: L=0
  END TYPE

  TYPE :: DT3(L)
    INTEGER( = ), LEN :: L=0
  END TYPE

  TYPE :: DT4(K)
    INTEGER(Kind=4, KIND :: K=0
  END TYPE

  TYPE :: DT5(K)
    INTEGER(Kind=4), KIND, LEN :: K=0
  END TYPE

  TYPE :: DT6(K)
    CHARACHER(LEN=4), KIND :: C=""
  END TYPE

  TYPE :: DT7(K)
    REAL(KIND=4), KIND :: K
  END TYPE

  TYPE :: DT8(K)
    LOGICAL(KIND=4), KIND :: K
  END TYPE

  TYPE :: DT9(L)
    COMPLEX(Kind=4), LEN :: L=0
  END TYPE


  END


