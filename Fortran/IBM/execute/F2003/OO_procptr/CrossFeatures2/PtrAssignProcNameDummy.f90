! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignProcNameDummy.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameDummy.f
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

  END MODULE


  PROGRAM PtrAssignProcNameDummy
  USE M
  IMPLICIT NONE
  PROCEDURE(IFun), POINTER :: ProcPtr
  PROCEDURE(IFun)          :: ExtFun

  INTERFACE
    FUNCTION IFun(Arg)
    CLASS(*)          :: Arg
    CLASS(*), POINTER :: IFun
    END FUNCTION
  END INTERFACE

  CALL IntSub(ModFun)
  CALL IntSub(ExtFun)

  ProcPtr => ModFun
  CALL IntSub(ProcPtr)

  ProcPtr => ExtFun
  CALL IntSub(ProcPtr)

  CONTAINS

  SUBROUTINE IntSub(Proc)

  PROCEDURE(IFun)          :: Proc
  PROCEDURE(IFun), POINTER :: Ptr

    Ptr => Proc
    SELECT TYPE ( As => Ptr(1_1) )
    TYPE IS (INTEGER(1))
      IF ( As .NE. 1_1 ) STOP 11
    CLASS DEFAULT
      STOP 12
    END SELECT

  END SUBROUTINE

  END

  FUNCTION ExtFun(Arg)
  CLASS(*)          :: Arg
  CLASS(*), POINTER :: ExtFun
    ALLOCATE(ExtFun, SOURCE=Arg)
  END FUNCTION


