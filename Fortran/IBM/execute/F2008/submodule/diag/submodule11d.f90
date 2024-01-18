!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule11d
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 6 December, 2012
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2011
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*    Call a subroutine declared in the host interface but defined differently
!*  in a descendant submodule expecting a severe message at compile time
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567891123456789112345678911234567891123456789112345678911234567890
MODULE m
implicit none
INTERFACE
  module subroutine sub(x, y)
    integer :: x
    real :: y
  end subroutine
END INTERFACE
END MODULE m

SUBMODULE (m) n
CONTAINS
  module subroutine sub(x, y)
    real :: x
    integer :: y
    print *, "oops!"
  end subroutine
END SUBMODULE n

PROGRAM submodule11d
USE m
implicit none
call sub()
END PROGRAM submodule11d
