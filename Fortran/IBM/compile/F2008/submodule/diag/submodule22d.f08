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
!*  Compile with -qlanglvl=[77std|90std|95std|2003std] which do not
!*   support submodule, expecting (L) messages.
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

  module subroutine sub2()
  end subroutine
END SUBMODULE n

PROGRAM
use m
END
