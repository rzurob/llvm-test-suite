!*********************************************************************
!*  ===================================================================
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

contains
  module function func()
    real :: func
  end function

  module subroutine sub()
  end subroutine
END MODULE m

SUBMODULE (m) n
CONTAINS
  module procedure func2
    real :: func2
  end

  module procedure sub2
  end
END SUBMODULE n
