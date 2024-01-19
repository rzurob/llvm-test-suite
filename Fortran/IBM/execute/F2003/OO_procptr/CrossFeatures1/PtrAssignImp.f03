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
!*  Implicit interface and explicitly typed
!*  or referenced as a function
!*  (304482)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: Id = 1
    END TYPE

    TYPE :: Child
      TYPE(Base) :: BaseComp
    END TYPE

  END MODULE

  FUNCTION ExtFun()
  USE M
  TYPE (Child) :: ExtFun
    ExtFun = Child(Base(Id=-1))
  END FUNCTION

  PROGRAM PtrAssignImp
  USE M
  IMPLICIT TYPE(Child)(C)

  INTERFACE
    FUNCTION ExtFun()
     IMPORT Child
     TYPE (Child) :: ExtFun
    END FUNCTION
  END INTERFACE

  PROCEDURE(TYPE(Child)),   POINTER :: ProcPtr
  TYPE (Child) :: V

  PROCEDURE(),  POINTER :: CProcPtr
  TYPE (Child) :: W

  ProcPtr => ExtFun
  V = ProcPtr()
  IF ( V%BaseComp%ID  .NE. -1 ) ERROR STOP 11

  CProcPtr => ExtFun
  W = CProcPtr()
  IF ( V%BaseComp%ID  .NE. -1 ) ERROR STOP 12

  END

