! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 02, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*    The selector is an associate name associating to a  non poly variable of derived types
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      TYPE(Base), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      TYPE(Base) :: BaseArr(3)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    TYPE(Child)  :: V = Child(BaseArr=(/Base(), Base(), Base() /))

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM HostAssocVarDer
  USE M, U => V
  IMPLICIT NONE

  ASSOCIATE (As0 =>  U%GetID())
  ASSOCIATE (As  =>  As0)
    IF ( As .NE. 2 ) ERROR STOP 60
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE (As =>  U%Base%GetID())
  ASSOCIATE (As =>  As)
    IF ( As .NE. 1 ) ERROR STOP 61
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( V => U )
  ASSOCIATE ( As => V )
    IF ( ASSOCIATED(As%BasePtr)) ERROR STOP 49
    IF ( As%GetID() .NE. 2) ERROR STOP 50
    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( As0 .NE. 2) ERROR STOP 51
       IF ( As1 .NE. 1) ERROR STOP 52
    END ASSOCIATE
    ASSOCIATE ( As2 => As%Base )
      IF ( As2%GetID() .NE. 1 ) ERROR STOP 53
    END ASSOCIATE

    U%BaseId = -1
    U%ChildId = -2
    IF ( As%BaseId .NE. -1)  ERROR STOP 71
    IF ( As%ChildId .NE. -2) ERROR STOP 72

  END ASSOCIATE
  END ASSOCIATE

  END
