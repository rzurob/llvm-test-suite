!*********************************************************************
!*  ===================================================================
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
!*    the interface of the dummy procedure is implicit and
!*    the procedure is referenced as  subroutine.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM intproc_arg_7
  INTEGER :: iii

  iii = 111
  CALL Extsub(intsub, 111)

  CONTAINS
    SUBROUTINE intsub(Arg)
    INTEGER :: Arg
      IF (iii .NE. ARG) ERROR STOP 11
      iii = -Arg
    END SUBROUTINE

  END

  SUBROUTINE Extsub(proc, Arg)
  EXTERNAL :: proc
  PROCEDURE(), POINTER :: procptr
  INTEGER :: Arg
    CALL proc(arg)
    CALL intsub(proc, -Arg)
    procptr => proc
    CALL procptr(Arg)
  CONTAINS

  SUBROUTINE intsub(proc, Arg)
  EXTERNAL :: proc
  INTEGER  :: Arg
    CALL proc(Arg)
  END SUBROUTINE

  END SUBROUTINE


