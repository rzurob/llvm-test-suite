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
!*  Test the basic functionality -- Mixed case
!*  pasing internal procedure around main/module/external procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  INTEGER :: i=0

  CONTAINS

    SUBROUTINE Modsub(proc)
    PROCEDURE() :: proc
    INTEGER :: j
      CALL proc(intsub)
      IF ( i .NE. 3 ) ERROR STOP 31
      IF ( j .NE. 3 ) ERROR STOP 32
    CONTAINS
      SUBROUTINE intsub(proc)
      PROCEDURE() :: proc
        i = 3
        j = 3
      END SUBROUTINE
    END SUBROUTINE

  END MODULE

  PROGRAM intproc_basic_4
  USE M
  External Extsub
  External Extsub1
  INTEGER :: j

  CALL Extsub(main_int)
  IF ( i .NE. 1 ) ERROR STOP 11
  IF ( j .NE. 1 ) ERROR STOP 12

  CALL Extsub1(main_int1)
  CALL Modsub(main_int1)

  CONTAINS

  SUBROUTINE main_int()
    i = 1
    j = 1
  END SUBROUTINE

  SUBROUTINE main_int1(proc)
  PROCEDURE() :: proc
    CALL proc()
  END SUBROUTINE

  END

  SUBROUTINE Extsub(proc)
  PROCEDURE() :: proc
    CALL proc()
  END SUBROUTINE

  SUBROUTINE Extsub1(proc)
  USE M
  PROCEDURE() :: proc
  INTEGER :: j
    CALL proc(intsub)
    IF ( i .NE. 2 ) ERROR STOP 21
    IF ( j .NE. 2 ) ERROR STOP 22
  CONTAINS
    SUBROUTINE intsub(proc)
    PROCEDURE() :: proc
      i = 2
      j = 2
    END SUBROUTINE
  END SUBROUTINE


