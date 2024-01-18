! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignProcNameMod.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameMod.f
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(*)          :: Arg
    CLASS(*), POINTER :: ModFun
      ALLOCATE(ModFun, SOURCE=Arg)
    END FUNCTION

    SUBROUTINE ModSub(Arg1, Arg2)
    PROCEDURE(ModFun) :: Arg1
    CLASS(*)          :: Arg2
      SELECT TYPE ( As => Arg1(Arg2) )
      TYPE IS (COMPLEX)
        IF ( As .NE. (1.0,-1.0) ) STOP 21
      CLASS DEFAULT
        STOP 22
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM PtrAssignProcNameMod
  USE M
  IMPLICIT NONE

  INTERFACE

    FUNCTION IFun(Arg)
    CLASS(*)          :: Arg
    CLASS(*), POINTER :: ModFun
    END FUNCTION

    SUBROUTINE ISub(Arg1, Arg2)
    IMPORT IFun
    PROCEDURE(IFun) :: Arg1
    CLASS(*)        :: Arg2
    END SUBROUTINE

  END INTERFACE

  PROCEDURE(ModFun), POINTER :: Ptr1
  PROCEDURE(ModSub), POINTER :: Ptr2

    Ptr1 => ModFun
    SELECT TYPE ( As => Ptr1("ABCD") )
    TYPE IS (CHARACTER(*))
      IF ( As .NE. "ABCD" ) STOP 11
    CLASS DEFAULT
      STOP 12
    END SELECT

    Ptr2 => ModSub
    CALL Ptr2(ModFun, (1.0,-1.0))

  END

