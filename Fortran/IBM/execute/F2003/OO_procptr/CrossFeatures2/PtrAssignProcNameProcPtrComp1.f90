! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 19, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!*
!*  The target is a procedure pointer component
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      PROCEDURE(GetBaseId), PASS, POINTER :: BasePtr=>NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      PROCEDURE(GetChildId), PASS, POINTER :: ChildPtr=>NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

  CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE



  PROGRAM PtrAssignProcNameProcPtrComp1
  USE M
  IMPLICIT NONE

  TYPE (Child) :: V

  V%BasePtr  => GetBaseId
  V%ChildPtr => GetChildId

  IF ( V%BasePtr()      .NE. V%Base%GetID() ) ERROR STOP 11
  IF ( V%Base%BasePtr() .NE. V%Base%GetID() ) ERROR STOP 12
  IF ( V%ChildPtr()     .NE. V%GetID() )      ERROR STOP 13

  ASSOCIATE (As => V)

    IF ( As%BasePtr()      .NE. V%Base%GetID() ) ERROR STOP 21
    IF ( As%Base%BasePtr() .NE. V%Base%GetID() ) ERROR STOP 22
    IF ( As%ChildPtr()     .NE. V%GetID() )      ERROR STOP 23

  END ASSOCIATE

  END

