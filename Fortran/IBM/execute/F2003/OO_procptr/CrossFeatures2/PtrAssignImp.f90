! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 27, 2005
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
!*  explicit  interface
!*  (ice-314926)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      PROCEDURE(GetBaseId), PASS, POINTER :: ProcPtr1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      PROCEDURE(GetChildId), PASS, POINTER :: ProcPtr2
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

  FUNCTION ExtFun()
  USE M
  TYPE (Child) :: ExtFun
    ExtFun = Child(BaseId=-1, ChildID=-2, ProcPtr1=GetBaseId, ProcPtr2=GetChildId)
  END FUNCTION

  PROGRAM PtrAssignImp
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION ExtFun()
     IMPORT Child
     TYPE (Child) :: ExtFun
    END FUNCTION
  END INTERFACE

  CLASS(Child), ALLOCATABLE :: V

  ALLOCATE(V, SOURCE=ExtFun())

  IF ( V%Base%GetID() .NE. V%Base%ProcPtr1() ) STOP 11
  IF ( V%GetID()     .NE. V%ProcPtr2()      ) STOP 12

  SELECT TYPE (V)
  TYPE IS (Child)
    V = V
  CLASS DEFAULT
    STOP  44
  END SELECT

  IF ( V%Base%GetID() .NE. V%Base%ProcPtr1() ) STOP 21
  IF ( V%GetID()     .NE. V%ProcPtr2()      ) STOP 22

  END

