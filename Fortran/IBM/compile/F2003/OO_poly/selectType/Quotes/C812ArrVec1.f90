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
!*    A select type construct is embeded in a class default block
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE, abstract :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    END TYPE

  END MODULE

  PROGRAM C812ArrVec1
  USE M
  IMPLICIT NONE

  CLASS(Base), ALLOCATABLE :: Ptr(:,:)

  ALLOCATE( Child :: Ptr(2:10, 3:12) )

  SELECT TYPE ( As => Ptr((/10,7,7,2/), (/12,3,3,12/)) )
    CLASS IS (Child)
      STOP 10
    TYPE IS (Child)
    CLASS DEFAULT
      SELECT TYPE ( As => As )
        TYPE IS (Child)
          As = Child()
      END SELECT
  END SELECT
  STOP 40

  END

