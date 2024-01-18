!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_15.f
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
!*    C1228 (R1221) A nonintrinsic elemental procedure shall not
!*    be used as an actual argument. 
!*    (388487)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 
  PROGRAM intproc_arg_15 

  procedure(intsub) :: extsub

  call extsub(eintsub)
  call intsub(eintsub)

  CONTAINS
    ELEMENTAL SUBROUTINE eintsub(i)
    integer, intent(in) :: i
    END SUBROUTINE

    SUBROUTINE intsub(proc)
    EXTERNAL :: proc 
    END SUBROUTINE
  END


