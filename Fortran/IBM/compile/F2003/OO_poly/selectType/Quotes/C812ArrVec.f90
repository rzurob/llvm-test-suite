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

    TYPE, abstract :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    END TYPE

  END MODULE

  PROGRAM C812ArrVec
  USE M
  IMPLICIT NONE

  CLASS(Base), POINTER :: Ptr(:,:)

  ALLOCATE( Child :: Ptr(2:10, 3:12) )

  SELECT TYPE ( As => Ptr((/10,7,7,2/), (/12,3,3,12/)) )
   class IS (child)
      STOP 10
    TYPE IS (Child)
       ASSOCIATE( As => As)
        As = Child()
      END ASSOCIATE
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

