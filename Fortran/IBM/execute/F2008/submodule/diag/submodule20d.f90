!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule20d
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
!*  Add an access modifier to a submodule.  Expect similar message to
!*   adding an access modifier to a module.
!*  
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890923456789092345678909234567890923456789092345678909234567890
PRIVATE MODULE m
integer a
implicit none
INTERFACE
  module subroutine intSub()
  end subroutine
  
  module function intFunc()
    integer intFunc
  end function
END INTERFACE

contains
  module function func()
    real :: func
  end function

  module subroutine sub()
  end subroutine
END MODULE m

PRIVATE SUBMODULE (m) n
CONTAINS
  module function func2()
    real :: func2
  end function

  module subroutine sub2()
  end subroutine
END SUBMODULE n
