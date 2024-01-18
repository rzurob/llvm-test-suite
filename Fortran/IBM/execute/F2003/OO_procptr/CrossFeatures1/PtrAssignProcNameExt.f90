! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 12, 2005
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
!*  (304401)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  INTERFACE

    FUNCTION IExtFun(Arg)
    INTEGER(8)          :: Arg(:)
    INTEGER(8), POINTER :: IExtFun(:)
    END FUNCTION

    SUBROUTINE IExtSub(Arg1, Arg2)
    IMPORT
    PROCEDURE(IExtFun) :: Arg1
    INTEGER(8)        :: Arg2(:)
    END SUBROUTINE

  END INTERFACE

  END MODULE


  PROGRAM PtrAssignProcNameExt
  USE M
  IMPLICIT NONE

  PROCEDURE(IExtFun),  POINTER :: Ptr
  PROCEDURE(IExtSub),  POINTER :: Ptr1
  PROCEDURE(IExtFun)           :: ExtFun
  PROCEDURE(IExtSub)           :: ExtSub
  INTEGER(8)                   :: As(3)

    Ptr => ExtFun
    As = Ptr((/3_8,2_8,1_8/))
    IF ( ANY(LBOUND(As) .NE. 1)) STOP 11
    IF ( ANY(UBOUND(As) .NE. 3)) STOP 12
    IF ( ANY(As .NE. (/3_8,2_8,1_8/)) ) STOP 13

    Ptr1 => ExtSub
    CALL Ptr1(Ptr, (/-3_8,-2_8,-1_8/))

  END

  FUNCTION ExtFun(Arg)
  INTEGER(8)          :: Arg(:)
  INTEGER(8), POINTER :: ExtFun(:)
    !ALLOCATE(ExtFun(2:1+SIZE(Arg)), SOURCE=Arg)  ! not 10.1
    ALLOCATE(ExtFun(2:1+SIZE(Arg)))
    ExtFun = Arg
  END FUNCTION

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  PROCEDURE(IExtFun)   :: Arg1
  INTEGER(8)           :: Arg2(:)
  INTEGER(8)           :: As(SIZE(Arg2))

    As = Arg1(Arg2)
    IF ( ANY(LBOUND(As) .NE. 1)) STOP 21
    IF ( ANY(UBOUND(As) .NE. (/SIZE(Arg2)/)) ) STOP 22
    IF ( ANY(As .NE. (/-3_8,-2_8,-1_8/) ) )    STOP 23

  END SUBROUTINE


