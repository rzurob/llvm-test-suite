!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule21d
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
!*  Reuse a submodule name in another submodule of the same host
!*   module, expecting a compile error.
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
    module subroutine sub2()
    end subroutine

    module function func2()
      real :: func2
    end function
  END INTERFACE
END MODULE m

SUBMODULE (m) n
CONTAINS
  module function func2()
    real :: func2
    func2 = 2
  end function
END SUBMODULE n

! as per defect 52106, currently the compiler cannot detect this and it will pass
SUBMODULE (m) n
CONTAINS
  module subroutine sub2()
  end subroutine
END SUBMODULE n

program p
use m
end program p
