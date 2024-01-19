! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Quotes/SltTypeZero.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 14, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*
!*   The type spec is specified with a type of zero size
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1    ! (4)
    END TYPE

    TYPE, EXTENDS(Base1) :: Child    ! (4)
    END TYPE

  END MODULE

  PROGRAM SltTypeZero
  USE M
  IMPLICIT NONE

  CLASS(Base(4)) ,ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=Child(4)())

  SELECT TYPE (Var)
   CLASS IS (Base(4))
     STOP 20
   TYPE IS (Base(4))
     STOP 21
   CLASS IS (Child(4))
     STOP 22
   TYPE IS (Child(4))
     PRINT*, "OK!"
   CLASS DEFAULT
     STOP 20
  END SELECT

  END

