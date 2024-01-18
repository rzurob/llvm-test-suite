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
!*    The selector is an associate name associating to a nonpoly dummy variable of derived types
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      TYPE(Base), POINTER :: BaseComp
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

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

  PROGRAM HostAssocVarDummy
  USE M
  IMPLICIT NONE

  TYPE(Child) :: V

  CALL Sub(V, V)

  CONTAINS

  SUBROUTINE Sub(Arg1, Arg2)
  IMPLICIT NONE
  TYPE(Child) :: Arg1, Arg2

  ASSOCIATE ( T1 => Arg1, T2 => Arg2 )
    IF ( T1%GetId() .NE. T2%GetId() ) STOP 30
    Arg1%ChildId = -2
    IF ( T1%GetId() .NE. T2%GetId() ) STOP 31
    IF ( T1%Base%GetId() .NE. T2%Base%GetId() ) STOP 32

    ASSOCIATE ( As1 => T1, As2 => T2 )
      Arg1%BaseId  = -1
      Arg2%ChildID = -2
      IF ( As1%BaseId  .NE. -1 ) STOP 40
      IF ( As2%ChildId .NE. -2 ) STOP 41
      IF ( As1%GetId() .NE. As2%GetId() ) STOP 42
      IF ( As1%Base%GetId() .NE. As2%Base%GetId() ) STOP 43
    END ASSOCIATE

    ASSOCIATE ( As1 => T1, As2 => T2 )
      As1%BaseId  = 1
      As2%ChildID = 2
      IF ( Arg1%BaseId  .NE. 1 ) STOP 50
      IF ( Arg2%ChildId .NE. 2 ) STOP 51
      IF ( Arg1%GetId() .NE. Arg2%GetId() ) STOP 52
      IF ( Arg1%Base%GetId() .NE. Arg2%Base%GetId() ) STOP 53
    END ASSOCIATE


  END ASSOCIATE


  END SUBROUTINE

  END
