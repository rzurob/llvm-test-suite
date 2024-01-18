!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram21d
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
!*  Name mismatch in the "END PROCEDURE" statement compared to the
!*   "MODULE PROCEDURE" statement
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
  ! name mismatch for a function
  module procedure func2
    func2 = 2
  end procedure func1

  ! name mismatch for a subroutine
  module procedure sub2
  end procedure sub1

END SUBMODULE n
