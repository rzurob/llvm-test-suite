! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignProcNameProcPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameProcPtr.f
!*
!*  DATE                       : Mar. 18, 2005
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
!*  The target is a procedure target
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION Fun(Arg)
        CHARACTER(5)  :: Arg
        CHARACTER(5)  :: Fun
      END FUNCTION
    END INTERFACE

  CONTAINS

  SUBROUTINE ModSub(ArgPtr)

    PROCEDURE(Fun), POINTER           :: Ptr
    PROCEDURE(Fun), POINTER, OPTIONAL :: ArgPtr
    CHARACTER(5)                      :: Str

    IF ( .NOT. PRESENT(ArgPtr) ) RETURN

    Ptr => ArgPtr

    Str = Ptr("12345")
    IF ( Str .NE. "12345" ) STOP 12

  END SUBROUTINE

  FUNCTION ModFun(Arg)
    CHARACTER(5)  :: Arg
    CHARACTER(5)  :: ModFun
      ModFun = Arg
  END FUNCTION

  FUNCTION ModFunArr(Arg)
  CHARACTER(5) :: Arg(:)
  CHARACTER(5) :: ModFunArr(SIZE(Arg))
      ModFunArr = Arg
  END FUNCTION

  END MODULE


  PROGRAM PtrAssignProcNameProcPtr
  USE M
  IMPLICIT NONE

  PROCEDURE(Fun),       POINTER :: Ptr1
  PROCEDURE(ModFunArr), POINTER :: Ptr2

  Ptr1 => ModFun
  CALL ModSub(Ptr1)

  Ptr2 => ModFunArr
  CALL IntSub(Ptr2)

  CONTAINS

  SUBROUTINE IntSub(ArgPtr)

  PROCEDURE(ModFunArr), POINTER :: Ptr
  PROCEDURE(ModFunArr), POINTER :: ArgPtr
  CHARACTER(5)            :: Str(10000)
  INTEGER                 :: I

    Ptr => ArgPtr
    Str = Ptr((/("54321", i=1, 10000) /) )
    IF ( ANY(Str .NE. "54321" )) STOP 22

  END SUBROUTINE

  END

