! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Misc13.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc13.f
!*
!*  DATE                       : Jun. 09, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!* Volatile - no external, no intent
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc13

  TYPE :: DT
    INTEGER :: I=1
  END TYPE

  PROCEDURE(), VOLATILE                :: ProcPtr1
  PROCEDURE(IntFun), VOLATILE          :: ProcPtr2
  PROCEDURE(IntFun), VOLATILE, POINTER :: ProcPtr3


  CONTAINS

  SUBROUTINE IntSub(Arg)
  PROCEDURE(IntFun), VOLATILE :: arg
  END SUBROUTINE

  SUBROUTINE IntSub1(Arg)
  PROCEDURE(IntFun), VOLATILE, INTENT(IN), POINTER :: Arg
  !PROCEDURE(IntFun),  INTENT(IN), POINTER :: Arg
  END SUBROUTINE

  FUNCTION IntFun(Arg)
  TYPE(DT) :: IntFun, Arg
    IntFun = Arg
  END FUNCTION

  END


