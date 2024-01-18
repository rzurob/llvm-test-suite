!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_2.f
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
!*    Diagnosis on Passing internal procedure to procedure dummy with 
!*    differnet characteristics. 
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 
  PROGRAM intproc_arg_2 

  CALL Intsub1(Intsub)
  CALL Intsub3(Intsub)  ! <-- no complaint, tolerant
  CALL Intsub5(Intsub4) 
  CALL Intsub6(Intfunc)

  CONTAINS

  SUBROUTINE Intsub()
  END SUBROUTINE

  SUBROUTINE Intsub1(arg)
  PROCEDURE(INTEGER) :: arg
  END SUBROUTINE

  SUBROUTINE Intsub2(arg)
  PROCEDURE(INTEGER) :: arg
  END SUBROUTINE

  SUBROUTINE Intsub3(arg)
  PROCEDURE(Intsub2) :: arg
  END SUBROUTINE

  ELEMENTAL SUBROUTINE Intsub4(arg)
  INTENT(IN) :: arg
  END SUBROUTINE

  SUBROUTINE Intsub5(arg)
  PROCEDURE() :: arg
  END SUBROUTINE

  INTEGER FUNCTION Intfunc()
   Intfunc = 0 
  END FUNCTION 

  SUBROUTINE Intsub6(arg)
  PROCEDURE(REAL) :: arg 
  END SUBROUTINE

  END

