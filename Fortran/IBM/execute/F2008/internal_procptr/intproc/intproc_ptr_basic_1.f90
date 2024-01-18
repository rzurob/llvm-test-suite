!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_ptr_basic_1.f
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
!*  Test the basic functionality --
!*     internal procedure in main as procedure target
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    INTEGER :: j=0
  END MODULE

  PROGRAM intproc_basic_1
  USE M
  INTEGER :: i=0
  PROCEDURE(), POINTER :: procptr
  PROCEDURE(INTEGER), POINTER :: procptr1

  procptr => Intsub
  CALL procptr()
  IF ( i .NE. -1 ) ERROR STOP 11

  procptr => Intsub1
  CALL procptr()
  IF ( j .NE. -1 ) ERROR STOP 12

  procptr1 => Intfunc
  j = -3
  IF ( procptr1() .NE. -3 ) ERROR STOP 13

  CONTAINS

  SUBROUTINE Intsub()
    i = -1
  END SUBROUTINE

  SUBROUTINE Intsub1()
    j = -1
  END SUBROUTINE

  FUNCTION Intfunc()
    Intfunc = j
  END FUNCTION

  END

