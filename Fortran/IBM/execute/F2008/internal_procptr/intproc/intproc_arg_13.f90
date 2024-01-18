!*********************************************************************
!*  ===================================================================
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
!*    The actual argument is a procedure pointer
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

  MODULE M
  USE M0
  USE M1
  PROCEDURE(), POINTER :: Procptr

  CONTAINS
  SUBROUTINE Modsub()

    iii = 0
    DO I=1, 100
      procptr => intset
      CALL Check(i-1)
      CALL intsub(Procptr, i)
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

  END SUBROUTINE
  END MODULE

  PROGRAM intproc_arg_13
  USE M
    CALL Modsub()
  END

