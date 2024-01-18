!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_6.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May 02, 2011
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
!*     Any procedure actual argument of pure procedure must be pure. 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 
  PROGRAM intproc_misc_6 

  CALL sub2(sub)
  CALL sub3(sub)

  CONTAINS

  SUBROUTINE sub()
  END SUBROUTINE

  PURE SUBROUTINE sub1()
  END SUBROUTINE

  PURE SUBROUTINE sub2(proc)
  PROCEDURE(sub1) :: proc
  END SUBROUTINE

  PURE SUBROUTINE sub3(proc)
  PROCEDURE(sub) :: proc
  END SUBROUTINE

  END

