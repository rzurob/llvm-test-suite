!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_basic_2.f
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
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*
!*  Test the basic functionality -- internal procedure as actual argument in module
!*   
!*
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

  CALL Intsubp(Intsub1)
  IF ( i .NE. -1 ) ERROR STOP 11

  CALL Intsubp(Intsub2)
  IF ( j .NE. -1 ) ERROR STOP 12

  IF ( Intfuncp(intfunc) .NE. -1 ) ERROR STOP 13

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
    Intfunc = -1
  END FUNCTION 

  END SUBROUTINE

  END MODULE
 
  PROGRAM intproc_basic_2
  USE M

  CALL Modsub()

  END

