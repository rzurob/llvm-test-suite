!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_1.f
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
!*  Test procedure argument asociation -- 
!*       Diagnosis on Passing internal procedure to data dummy
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 
  PROGRAM intproc_arg_1 

  CALL Intsub(Intsub)
  CALL Intsub1(Intsub)
  CALL Intsub2(Intsub)
  CALL Intsub3(Intsub)
  CALL Intsub4(Intsub)
  CALL Intsub5(Intsub)

  CONTAINS

  SUBROUTINE Intsub()
  END SUBROUTINE

  SUBROUTINE Intsub1(arg)
  END SUBROUTINE

  SUBROUTINE Intsub2(arg)
  INTEGER, POINTER :: arg
  END SUBROUTINE

  SUBROUTINE Intsub3(arg)
  POINTER :: arg
  END SUBROUTINE

  SUBROUTINE Intsub4(arg)
  PROCEDURE(INTEGER) :: arg
  END SUBROUTINE

  SUBROUTINE Intsub5(arg)
  PROCEDURE() :: arg
   r = arg()
  END SUBROUTINE

  END

