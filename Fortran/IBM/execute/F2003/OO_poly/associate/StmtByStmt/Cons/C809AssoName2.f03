! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     The associate name must only be declared once in the ASSOCIATE statement
!*     Selector is a dummy with the same name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: Id = 0
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM C809AssoName1
  USE M
  IMPLICIT NONE
  TYPE(Child) :: V = Child(1)

    V = F(V)

    CONTAINS

    FUNCTION F(As)
    TYPE(Child) :: As, F

    F = Child(1)

    ASSOCIATE ( As => As  )
      IF (As%Id .NE. 1) ERROR STOP 50
      IF (As%GetId() .NE. 1) ERROR STOP 51
      As%Id = 2

      ASSOCIATE ( As => As  )
        IF (As%Id .NE. 2) ERROR STOP 52
        IF (As%GetId() .NE. 2) ERROR STOP 53
      END ASSOCIATE
    END ASSOCIATE

    IF (As%Id .NE. 2) ERROR STOP 52
    IF (As%GetId() .NE. 2) ERROR STOP 53

    END FUNCTION

  END
