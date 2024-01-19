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
!*    The selector is a non poly allocatable variable of derived types
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER, ALLOCATABLE :: BaseId
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER, ALLOCATABLE  :: ChildId
      TYPE(Base)            :: BaseComp
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
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM VarAlloc
  USE M
  IMPLICIT NONE

  TYPE(Child), ALLOCATABLE   :: U

  ALLOCATE(U)
  ALLOCATE(U%ChildId, SOURCE=2)
  ALLOCATE(U%BASE%BaseId, SOURCE=1)

  ASSOCIATE ( As => U )
    IF ( As%GetID() .NE. 2) ERROR STOP 50
    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( As0 .NE. 2) ERROR STOP 51
       IF ( As1 .NE. 1) ERROR STOP 52
    END ASSOCIATE
    ASSOCIATE ( As2 => As%Base )
      IF ( As2%GetID() .NE. 1 ) ERROR STOP 53
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE (As =>  U%GetID())
    IF ( As .NE. 2 ) ERROR STOP 60
  END ASSOCIATE

  ASSOCIATE (As =>  U%Base%GetID())
    IF ( As .NE. 1 ) ERROR STOP 61
  END ASSOCIATE

  ASSOCIATE (As =>  U%BaseComp)
    ALLOCATE(As%BaseId, SOURCE=1)
    IF ( As%BaseId .NE. 1 ) ERROR STOP 61
  END ASSOCIATE

  END
