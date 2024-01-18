!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_3.f
!*
!*  DATE                       : April 21 2011
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
!*  Test the dummy procedure --
!*    A pure efective argument may be associated with a dummy argument
!*    that is not pure
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    INTEGER :: j=0
  CONTAINS

  PURE SUBROUTINE Intset(j, i)
  INTEGER, INTENT(OUT):: j
  INTEGER, INTENT(IN) :: i
    j = i
  END SUBROUTINE

  SUBROUTINE Intcallsub(proc,i, j)
  procedure() :: proc
  call proc(j, i)
  END SUBROUTINE

  SUBROUTINE Intcheck(i)
    IF ( j .NE. i) ERROR STOP 11
  END SUBROUTINE


  END MODULE

  PROGRAM intproc_arg_3
  USE M

  DO i=1, 1000
    CALL Intcallsub(Intset, j, i)
    CALL Intcheck(i)
  END DO


  END

