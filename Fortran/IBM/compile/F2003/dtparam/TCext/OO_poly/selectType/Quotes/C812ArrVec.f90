! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Quotes/C812ArrVec.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C812
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
!*    The selector is an array section  with a vector subscript
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE, abstract :: Base(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
    END TYPE

  END MODULE

  PROGRAM C812ArrVec
  USE M
  IMPLICIT NONE

  CLASS(Base(4)), POINTER :: Ptr(:,:)

  ALLOCATE( Child(4) :: Ptr(2:10, 3:12) )

  SELECT TYPE ( As => Ptr((/10,7,7,2/), (/12,3,3,12/)) )
   class IS (Child(4))
      STOP 10
    TYPE IS (Child(4))
       ASSOCIATE( As => As)
        As = Child(4)()
      END ASSOCIATE
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

