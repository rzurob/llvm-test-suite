!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_14.f
!*
!*  DATE                       : April 27 2011
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
!*    The actual argument is an internal function reference
!*    which returns a procedure pointer
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M0
    INTEGER :: iii
  END MODULE

  MODULE M1
  USE M0
    CONTAINS

    SUBROUTINE Check(Arg)
    INTEGER Arg
      IF ( iii .NE. Arg) ERROR STOP 11
    END SUBROUTINE

  END MODULE

  SUBROUTINE Extsub()
  USE M0
  USE M1
  PROCEDURE(), POINTER  :: Procptr

    iii = 0
    DO I=1, 100
      procptr => intset
      CALL Check(i-1)
      CALL intsub(Intfunc(Procptr), i)
      CALL Check(i)
    END DO

    CONTAINS

    SUBROUTINE intsub(Proc, Arg)
    PROCEDURE(intset), POINTER, INTENT(IN), OPTIONAL :: Proc
    INTEGER :: Arg
      IF ( PRESENT(proc) ) THEN
        CALL Proc(Arg)
      ELSE
        ERROR STOP 12
      END IF
    END SUBROUTINE

    SUBROUTINE intset(Arg)
    INTEGER :: Arg
      iii = Arg
    END SUBROUTINE

    FUNCTION intfunc(proc)
    PROCEDURE(intset), OPTIONAL  :: Proc
    PROCEDURE(intset), POINTER :: Intfunc
      IF ( PRESENT(Proc) ) THEN
        Intfunc => Proc
      ELSE
        ERROR STOP 12
      END IF
    END FUNCTION

  END SUBROUTINE

  PROGRAM intproc_arg_14
  EXTERNAL Extsub
    CALL Extsub()
  END

