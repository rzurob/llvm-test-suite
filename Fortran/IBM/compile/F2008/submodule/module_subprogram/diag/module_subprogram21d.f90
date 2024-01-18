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
  module procedure func2
    real :: func2
    func2 = 2
  end
END SUBMODULE n

! as per RTC defect 52106: currently the compiler cannot detect this, and compilation will pass
SUBMODULE (m) n
CONTAINS
  module procedure sub2
  end
END SUBMODULE n

Program p
use m
end program p
