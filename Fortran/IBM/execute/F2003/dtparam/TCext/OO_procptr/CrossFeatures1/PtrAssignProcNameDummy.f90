! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv /tstdev/OO_procptr/CrossFeatures1/PtrAssignProcNameDummy.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

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

  TYPE ::DT0(N1,K1)    ! (20,1)
    INTEGER, KIND            :: K1
    INTEGER, LEN             :: N1
    INTEGER(K1), ALLOCATABLE :: IArr(:)
  END TYPE

  TYPE :: DT(K2,N2,K3)    ! (4,20,1)
    INTEGER, KIND    :: K2,K3
    INTEGER, LEN     :: N2
    TYPE(DT0(N2,K3)) :: Base
  END TYPE

  CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(DT(4,*,1)) :: Arg
    TYPE(DT(4,20,1)) :: ModFun
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
      TYPE(DT(4,*,1)) :: Arg
      TYPE(DT(4,20,1)) :: IFun
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
  TYPE (DT(4,20,1))                :: V(3)
  INTEGER                  :: i

    Ptr => Proc
    V = Ptr(DT(4,20,1)(DT0(20,1)((/(INT(i, 1), i=1,10000)/))))
    IF (ANY( V(1)%Base%IArr .NE. (/(INT(i, 1), i=1,10000)/) ) ) ERROR STOP 11
    IF (ANY( V(2)%Base%IArr .NE. (/(INT(i, 1), i=1,10000)/) ) ) ERROR STOP 12
    IF (ANY( V(3)%Base%IArr .NE. (/(INT(i, 1), i=1,10000)/) ) ) ERROR STOP 13

  END SUBROUTINE

  END

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT(4,*,1)) :: Arg
  TYPE(DT(4,20,1)) :: ExtFun
    ExtFun = Arg
  END FUNCTION


