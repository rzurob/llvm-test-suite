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
!*     Selector is structure constructor with the same name as associate name
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

  PROGRAM C809AssoName4
  USE M
  IMPLICIT NONE
  TYPE(Child) :: V = Child(-1)

    ASSOCIATE ( Child => Child(Id=1)  )
      IF (Child%Id      .NE. 1) ERROR STOP 50
      IF (Child%GetId() .NE. 1) ERROR STOP 51

      ASSOCIATE ( Child => Child  )
        IF (Child%Id      .NE. 1) ERROR STOP 52
        IF (Child%GetId() .NE. 1) ERROR STOP 53
      END ASSOCIATE
    END ASSOCIATE

    ASSOCIATE ( V => V  )
      IF (V%Id      .NE. -1) ERROR STOP 60
      IF (V%GetId() .NE. -1) ERROR STOP 61

      V%Id = -2

      ASSOCIATE ( V => V  )
        IF (V%Id      .NE. -2) ERROR STOP 62
        IF (V%GetId() .NE. -2) ERROR STOP 63
      END ASSOCIATE
    END ASSOCIATE


  END

