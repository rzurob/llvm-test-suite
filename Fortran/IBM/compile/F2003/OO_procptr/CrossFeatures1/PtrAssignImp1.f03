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
!*  ()
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

  SUBROUTINE ExtSub(Arg)
  USE M
  TYPE (Child) :: Arg
    Arg = Child(Base(-1))
  END SUBROUTINE

  PROGRAM PtrAssignImp1
  USE M
  IMPLICIT TYPE(Child)(C)

  INTERFACE
    SUBROUTINE ExtSub(Arg)
     IMPORT Child
     TYPE (Child) :: Arg
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(TYPE(Child)),  POINTER :: ProcPtr
  PROCEDURE(),             POINTER :: CProcPtr

  ProcPtr => ExtSub
  CProcPtr => ExtSub
  PRINT*, CProcPtr(Child(Base(1)))
  END
