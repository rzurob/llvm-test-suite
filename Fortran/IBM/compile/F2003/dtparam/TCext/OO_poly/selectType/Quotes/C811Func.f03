! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Quotes/C811Func.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a function call  without ssociate-name =>
!*
!*    (Passing-301383)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM C811Func
  IMPLICIT NONE

  TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  SELECT TYPE ( Fun(Base(4)()) )
    TYPE IS (Base(4))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(Base(4)) :: Arg
  CLASS(Base(4)), POINTER :: Fun
    ALLOCATE(Fun)
    SELECT TYPE( Fun )
      TYPE IS (Base(4))
        Fun=Arg
      CLASS DEFAULT
        STOP 22
    END SELECT
  END FUNCTION

  END
