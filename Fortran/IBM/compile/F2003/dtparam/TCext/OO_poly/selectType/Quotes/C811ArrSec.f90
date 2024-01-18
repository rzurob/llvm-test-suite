! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Quotes/C811ArrSec.f
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
!*    The selector is an array section  without ssociate-name =>
!*
!*    (Wrong check)
!*   Relax the err msg check to the current err msg
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
    CONTAINS
      PROCEDURE, PASS   :: GetBase
    END TYPE

    CONTAINS

    FUNCTION GetBase(Arg)
    CLASS(Child(4))              :: Arg
    CLASS(Base(4)), ALLOCATABLE  :: GetBase(:)
      ALLOCATE(GetBase(3))
      SELECT TYPE (GetBase)
        TYPE IS (Base(4))
          GetBase = Arg%Base
        CLASS DEFAULT
          STOP 33
      END SELECT
    END FUNCTION

  END MODULE

  PROGRAM C811ArrSec
  USE M
  IMPLICIT NONE

  CLASS(Base(4)), POINTER :: Ptr(:,:)

  ALLOCATE( Child(4) :: Ptr(2:10, 3:12) )

  SELECT TYPE ( Ptr(::2, ::1) )
    TYPE IS (Base(4))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

