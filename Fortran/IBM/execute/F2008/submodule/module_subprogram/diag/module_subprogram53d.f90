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
!*  Redefine a function or subroutine already defined with the module
!*   function or module subroutine keywords
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
    module subroutine sub2(x)
      integer x
    end subroutine

    module function func2(x)
      integer x
      real :: func2
    end function
  END INTERFACE
END MODULE m

SUBMODULE (m) n
CONTAINS
  module function func2(x)
    integer x
    func2 = 2 * x
  end function func2

  module subroutine sub2(x)
    integer x
    a = x
  end subroutine sub2

END SUBMODULE n

SUBMODULE (m:n) o
CONTAINS
  ! for a function
  module procedure func2
    func2 = 3 * x
  end procedure func2

  ! for a subroutine
  module procedure sub2
    a = 2 * x
  end procedure sub2

END SUBMODULE o
