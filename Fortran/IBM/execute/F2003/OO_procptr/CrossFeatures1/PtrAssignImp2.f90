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
!*  If proc-pointer-object has an implicit interface and is referenced
!*  as a subroutine, proc-target shall be a subroutine.
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

  PROGRAM PtrAssignImp2
  USE M
  IMPLICIT TYPE(Child)(C)

  INTERFACE
    SUBROUTINE Extsub(Arg)
     IMPORT Child
     TYPE (Child) :: arg
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(),  POINTER :: ProcPtr
  PROCEDURE(),  POINTER :: CProcPtr
  TYPE(child)           :: V,W

  ProcPtr => ExtSub
  CALL  ProcPtr(V)
  IF ( V%BaseComp%ID   .NE. -1 ) ERROR STOP 11

  CProcPtr => ExtSub
  CALL  CProcPtr(W)
  IF ( W%BaseComp%ID   .NE. -1 ) ERROR STOP 21

  END

