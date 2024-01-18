!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_5.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : April 21 2011
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*
!*  Test the argument association --
!*    The dummy procedure's interface is implicit.
!*   
!*  (388373)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    REAL :: R(1000)
  CONTAINS

  SUBROUTINE Intcallsub(proc, i)
  procedure(real) :: proc
   R(i) = proc(REAL(i)) 
  END SUBROUTINE

  SUBROUTINE Intcallsub1(proc, i)
  procedure() :: proc
   R(i) = proc(REAL(i)) 
  END SUBROUTINE

  SUBROUTINE Intcheck(i)
  logical  precision_r4
    IF ( .NOT. precision_r4(SIN(REAL(i)), R(i))) ERROR STOP 11 
  END SUBROUTINE


  END MODULE
 
  PROGRAM intproc_arg_5
  USE M
  INTRINSIC SIN
  PROCEDURE(REAL), POINTER :: P1
  EXTERNAL :: p2
  POINTER  :: P2
  !PROCEDURE() :: P2

  DO I=1, 1000
    P1 => SIN
    CALL Intcallsub(P1, i)
    CALL Intcheck(i)
  END DO

  DO I=1000, i, -1
    P2 => SIN
    CALL Intcallsub1(P2, i)
    CALL Intcheck(i)
  END DO

  END

