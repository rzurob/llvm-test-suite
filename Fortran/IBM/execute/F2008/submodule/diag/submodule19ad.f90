!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule19d
!*
!*  DATE                       : 6 December, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*    MODULE appears in the function-stmt or subroutine-stmt of a
!*  module subprogram or a non-abstract interface body that is
!*  declared in the scoping unit of a module or submodule
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890923456789092345678909234567890923456789092345678909234567890
MODULE m
integer a
implicit none
INTERFACE
  module subroutine intSub()
  end subroutine

  module function intFunc()
    integer intFunc
  end function
END INTERFACE

END MODULE m

SUBMODULE (m) n
CONTAINS
  module function func2()
    real :: func2
  end function

  module subroutine sub2()
  end subroutine
END SUBMODULE n
