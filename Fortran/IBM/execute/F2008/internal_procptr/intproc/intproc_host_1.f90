!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_host_1.f
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
!*  Test host instance --
!*    Access the environment in external procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  SUBROUTINE Extsub(extsub_arg)
  PROCEDURE(), POINTER  :: Procptr
  INTEGER :: iii,get_iii , extsub_arg

  INTERFACE
    SUBROUTINE Extcall(Proc, Arg1, Arg2)
    PROCEDURE() :: Proc
    INTEGER :: Arg1, Arg2
    END SUBROUTINE
  END INTERFACE

    iii = 0
    extsub_arg = 0

    DO I=1, 100
      procptr => intset
      CALL Extcall(Intfunc(Procptr), i, 0)
      CALL Extcall(Procptr, i, 0)
      extsub_arg = i
      CALL Extcall(Check, i, get_iii)
      IF ( get_iii .NE. i) ERROR STOP 15
    END DO

    CONTAINS

    SUBROUTINE intset(Arg, argx)
    INTEGER :: Arg, argx
      iii = Arg
    END SUBROUTINE

    FUNCTION intfunc(proc)
    PROCEDURE(intset), OPTIONAL  :: Proc
    PROCEDURE(intset), POINTER :: Intfunc
      IF ( PRESENT(Proc) ) THEN
        Intfunc => Proc
      ELSE
        ERROR STOP 13
      END IF
    END FUNCTION

    SUBROUTINE Check(Arg1, Arg2)
    INTEGER Arg1, Arg2
      IF ( extsub_arg .NE. Arg1) ERROR STOP 10
      IF ( iii .NE. Arg1) ERROR STOP 11
      Arg2 = iii
      !IF ( .NOT. Associated(Procptr, intset)) ERROR STOP 12 !<-- 390181
      IF ( .NOT. Associated(Procptr)) ERROR STOP 12
    END SUBROUTINE

  END SUBROUTINE

  SUBROUTINE Extcall(Proc, Arg1, Arg2)
  PROCEDURE() :: Proc
  INTEGER :: Arg1, Arg2
     CALL Proc(Arg1, Arg2)
  END SUBROUTINE

  PROGRAM intproc_host_1
  EXTERNAL Extsub
    CALL Extsub(-1)
  END

