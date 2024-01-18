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
  module procedure sub
    real :: x
    integer :: y
    print *, "oops!"
  end
END SUBMODULE n

PROGRAM module_subprogram11d
USE m
implicit none
call sub()
END PROGRAM module_subprogram11d
