!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 21 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test the basic functionality -- internal procedure as actual argument in module
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0
  INTEGER :: j=0
  END MODULE

  MODULE M
  USE M0

  CONTAINS

  SUBROUTINE Modsub()
  INTEGER :: i=0
  PROCEDURE(), POINTER :: procptr
  PROCEDURE(Intfunc), POINTER :: procptr1

  procptr => Intsub1
  CALL procptr()
  IF ( i .NE. -1 ) ERROR STOP 11

  procptr => Intsub2
  CALL procptr()
  IF ( j .NE. -1 ) ERROR STOP 12

  procptr1 => intfunc
  IF ( procptr1() .NE. -1 ) ERROR STOP 13

  CONTAINS

  SUBROUTINE Intsub1()
    i = -1
  END SUBROUTINE

  SUBROUTINE Intsubp(proc)
  PROCEDURE() :: proc
    CALL proc()
  END SUBROUTINE

  SUBROUTINE Intsub2()
    j = -1
  END SUBROUTINE

  FUNCTION  Intfuncp(proc)
  PROCEDURE(INTEGER) :: proc
    Intfuncp = proc()
  END FUNCTION

  FUNCTION Intfunc()
    Intfunc = i
  END FUNCTION

  END SUBROUTINE

  END MODULE

  PROGRAM intproc_ptr_basic_2
  USE M

  CALL Modsub()

  END
