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
!*  If proc-target and proc-pointer-object are functions,
!*  they shall have the same type; corresponding type parameters
!*  shall either both be deferred or both have the same value.
!*
!*  (Mem Fault)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: Base
      INTEGER :: Id = 1
    END TYPE

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base), POINTER :: ExtFun(:)
  TYPE(Base)          :: Arg(:)
    !ALLOCATE(ExtFun(2:1+SIZE(Arg)), SOURCE=Arg)
    ALLOCATE(ExtFun(2:1+SIZE(Arg)))
    ExtFun = Arg
  END FUNCTION

  PROGRAM PtrAssignImp5
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION ExtFun(Arg)
      IMPORT
      TYPE(Base), POINTER :: ExtFun(:)
      TYPE(Base)          :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(ExtFun),   POINTER :: ProcPtr
  TYPE (Base) :: V(3) = Base(0)

  ProcPtr => ExtFun
  V = ProcPtr((/Base(-1), Base(-1), Base(-1)/) )

  IF ( ANY(V%ID   .NE. -1 )) ERROR STOP 21

  END
