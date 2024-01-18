!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram51d
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
!*  Syntax error "MODULE PROCEDURE foo()"
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
  ! for a function
  module procedure func2()
    func2 = 2
  end procedure func2

  ! for a subroutine
  module procedure sub2()
  end procedure sub2

END SUBMODULE n
