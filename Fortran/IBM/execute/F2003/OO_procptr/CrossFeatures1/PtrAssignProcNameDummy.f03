! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 13, 2005
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
!*  (304414/300958/319887)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE ::DT0
    INTEGER(1), ALLOCATABLE :: IArr(:)
  END TYPE

  TYPE :: DT
    TYPE(DT0) :: Base
  END TYPE

  CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(DT) :: Arg
    TYPE(DT) :: ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM PtrAssignProcNameDummy
  USE M
  IMPLICIT NONE
  PROCEDURE(IFun)          :: ExtFun
  PROCEDURE(IFun), POINTER :: Ptr

  INTERFACE
    FUNCTION IFun(Arg)
      IMPORT
      TYPE(DT) :: Arg
      TYPE(DT) :: IFun
    END FUNCTION
  END INTERFACE

  CALL IntSub(ModFun)
  CALL IntSub(ExtFun)

  Ptr => ModFun
  CALL IntSub(Ptr)

  Ptr => ExtFun
  CALL IntSub(Ptr)

  CONTAINS

  SUBROUTINE IntSub(Proc)

  PROCEDURE(IFun)          :: Proc
  PROCEDURE(IFun), POINTER :: Ptr
  TYPE (DT)                :: V(3)
  INTEGER                  :: i

    Ptr => Proc
    V = Ptr(DT(DT0((/(INT(i, 1), i=1,10000)/))))
    IF (ANY( V(1)%Base%IArr .NE. (/(INT(i, 1), i=1,10000)/) ) ) ERROR STOP 11
    IF (ANY( V(2)%Base%IArr .NE. (/(INT(i, 1), i=1,10000)/) ) ) ERROR STOP 12
    IF (ANY( V(3)%Base%IArr .NE. (/(INT(i, 1), i=1,10000)/) ) ) ERROR STOP 13

  END SUBROUTINE

  END

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT) :: Arg
  TYPE(DT) :: ExtFun
    ExtFun = Arg
  END FUNCTION


