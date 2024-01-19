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
!*    The actual procedure argument has the same generic name
!*    (Internal proc in procedure stmt -- 388396)
!*    (Not in 14.1 content. TC has to change to avoid this)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0
  INTEGER :: iii
  END MODULE

  MODULE M
  USE M0
  END MODULE

  PROGRAM intproc_arg_8
  USE M
  PROCEDURE(Intsub), POINTER :: procptr1
  PROCEDURE(Intsub3), POINTER :: procptr2

  !INTERFACE IntSub
  !  PROCEDURE Intsub
  !  PROCEDURE Intsub1
  !END INTERFACE

  INTERFACE procptr
    PROCEDURE Procptr1
    PROCEDURE Procptr2
  END INTERFACE

  Procptr1 => Intsub
  Procptr2 => Intsub3

! CALL IntSub(Intsub1, -1)
  CALL Procptr(Intsub1, -1)
  IF ( iii .NE. -1) ERROR STOP 11

  !CALL IntSub(Intsub2, -1.)
  CALL Procptr(Intsub2, -1.)
  IF ( iii .NE. 1) ERROR STOP 12

  CONTAINS
    SUBROUTINE intsub1(Arg)
    INTEGER :: Arg
      iii = Arg
    END SUBROUTINE

    SUBROUTINE intsub2(Arg)
    REAL :: Arg
      iii = -Arg
    END SUBROUTINE

    SUBROUTINE intsub(Proc,Arg)
    PROCEDURE() Proc
    INTEGER :: Arg
      CALL Proc(Arg)

    END SUBROUTINE

    SUBROUTINE intsub3(Proc,Arg)
    PROCEDURE() Proc
    REAL :: Arg
      CALL Proc(Arg)
    END SUBROUTINE

  END



