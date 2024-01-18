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
!*  (ice/315447)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE :: Base
      INTEGER :: BaseId = 1
      PROCEDURE(IExt), NOPASS, POINTER :: ProcPtr0
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      PROCEDURE(IExt), NOPASS, POINTER :: ProcPtr1
    END TYPE

!   INTERFACE
!     SUBROUTINE IExt(Arg)
!      IMPORT Base
!      CLASS(Base) :: arg
!   END SUBROUTINE
!   END INTERFACE

  CONTAINS
      SUBROUTINE IExt(Arg)
       CLASS(Base) :: arg
    END SUBROUTINE

  END MODULE

  SUBROUTINE ExtSub(Arg)
  USE M
  CLASS (Base) :: Arg
    SELECT TYPE (Arg)
    TYPE IS (Base)
      Arg = Base(BaseID =-1, ProcPtr0=IExt)
    TYPE IS (Child)
      Arg = Child(BaseId=-1, ChildID=-2,  ProcPtr0=IExt,  ProcPtr1=IExt)
    END SELECT
  END SUBROUTINE

  PROGRAM PtrAssignImp2
  USE M
  IMPLICIT TYPE(Base)(C)

  PROCEDURE(IExt)         :: ExtSub
  PROCEDURE(IExt),POINTER :: ProcPtr
  PROCEDURE(),  POINTER   :: CProcPtr
  TYPE(Base )               :: V1
  TYPE(child)               :: V2
  CLASS(Base),  ALLOCATABLE :: V3
  CLASS(Child), ALLOCATABLE :: V4

  ProcPtr => ExtSub

  CALL  ProcPtr(V1)
  IF ( V1%BaseID      .NE. -1 ) ERROR STOP 11
  IF ( .NOT. ASSOCIATED(V1%ProcPtr0, IExt) ) ERROR STOP 12

  CALL  ProcPtr(V2)
  IF ( V2%BaseID      .NE. -1 ) ERROR STOP 21
  IF ( V2%Base%BaseID .NE. -1 ) ERROR STOP 22
  IF ( V2%ChildID     .NE. -2 ) ERROR STOP 23
  IF ( .NOT. ASSOCIATED(V2%ProcPtr0, IExt) ) ERROR STOP 24
  IF ( .NOT. ASSOCIATED(V2%ProcPtr1, IExt) ) ERROR STOP 25

  ALLOCATE(V3)
  CALL  ProcPtr(V3)
  IF ( V3%BaseID      .NE. -1 ) ERROR STOP 31
  IF ( .NOT. ASSOCIATED(V3%ProcPtr0, IExt) ) ERROR STOP 32

  ALLOCATE(V4)
  CALL  ProcPtr(V4)
  IF ( V4%BaseID      .NE. -1 ) ERROR STOP 41
  IF ( V4%Base%BaseID .NE. -1 ) ERROR STOP 42
  IF ( V4%ChildID     .NE. -2 ) ERROR STOP 43
  IF ( .NOT. ASSOCIATED(V4%ProcPtr0, IExt) ) ERROR STOP 44
  IF ( .NOT. ASSOCIATED(V4%ProcPtr1, IExt) ) ERROR STOP 45


  END

