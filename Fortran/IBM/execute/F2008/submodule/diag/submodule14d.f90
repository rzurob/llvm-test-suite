!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule14d
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 6 December, 2012
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*    Protected variable mutability:
!*  Positive diagnostic: module subroutine or function should be
!*    allowed to modify the host scope protected variable.
!*
!*  Negative diagnostic: a program that use-associates the host module
!*    should not be able to modify the host scope protected variable.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567891423456789142345678914234567891423456789142345678914234567890
MODULE m
implicit none
integer, protected :: a
INTERFACE
  module subroutine sub()
  end subroutine
  
  module function func()
    integer :: func
  end function
END INTERFACE
END MODULE m

SUBMODULE (m) n
CONTAINS
  module subroutine sub()
    a = 32
  end subroutine

  module function func()
    integer :: func
    a = 33
    func = a
  end function
END SUBMODULE n

PROGRAM submodule14d
USE m
implicit none
integer :: b
call sub()
if (.not.a.eq.32) error stop 66
if (.not.func().eq.33) error stop 77

a = 2 ! should cause severe error

END PROGRAM submodule14d
