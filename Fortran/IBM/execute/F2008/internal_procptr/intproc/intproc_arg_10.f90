!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_10.f
!*
!*  DATE                       : April 25 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test the argument association --
!*    The dummy is procedure pointer and
!*    The actual argument is a reference to a function that returns a procedure
!*    pointer associated with an internal procedure.
!*
!*   (388421, 388838, 388841)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  CONTAINS
  SUBROUTINE Modsub()
    INTEGER, SAVE :: iii

    iii = 0
    DO I=1, 100
      CALL Check(i-1)
      CALL intsub(Intfunc(intset), i)
      CALL Check(i)
    END DO

    IF ( Associated(Func())) ERROR STOP 12

    CONTAINS

    SUBROUTINE intsub(Proc, Arg)
    PROCEDURE(intset), POINTER, INTENT(IN) :: Proc
    INTEGER :: Arg
      CALL Proc(Arg)
    END SUBROUTINE

    SUBROUTINE intset(Arg)
    INTEGER :: Arg
      iii = Arg
    END SUBROUTINE

    SUBROUTINE Check(Arg)
    INTEGER Arg
      IF ( iii .NE. Arg) ERROR STOP 11
    END SUBROUTINE

    FUNCTION intfunc(Proc)
    PROCEDURE(intset), POINTER :: Intfunc
    PROCEDURE(intset), POINTER, INTENT(IN) :: Proc
      Intfunc => Proc
    END FUNCTION

    FUNCTION Func()
    PROCEDURE(), POINTER :: Func
      Func => NULL()
    END FUNCTION

  END SUBROUTINE
  END MODULE

  PROGRAM intproc_arg_10
  USE M
    CALL Modsub()
  END



