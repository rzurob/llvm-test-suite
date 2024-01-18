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
!*    The selector is an associate name associating to a poly pointer variable of derived types
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
      TYPE(Child), POINTER :: ChildPtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM HostAssocVarPolyPtr
  USE M
  IMPLICIT NONE

  TYPE(Base),  TARGET      :: V
  TYPE(Child), TARGET      :: U
  TYPE(Base),  POINTER    ::  BasePtr
  TYPE(Child), POINTER     :: ChildPtr

  BasePtr => V
  ChildPtr => U

  ASSOCIATE ( T1  => BasePtr, T2 => ChildPtr, T3 => V, T4 => U  )
  ASSOCIATE ( As1 => T1, As2 => T2  )
    IF ( As1%GetID() .NE. 1) STOP 50
    IF ( As2%GetID() .NE. 2) STOP 51

    T1%BasePtr  => T3
    T2%ChildPtr => T4

    IF ( .NOT. ASSOCIATED( As1%BasePtr, V) )  STOP 60
    IF ( .NOT. ASSOCIATED( As2%ChildPtr, U) ) STOP 61

    U%BaseId  = -1
    U%ChildId = -2

    IF ( As2%Base%GetID() .NE. -1) STOP 70
    IF ( As2%GetID()      .NE. -2) STOP 71

  END ASSOCIATE
  END ASSOCIATE


  END
