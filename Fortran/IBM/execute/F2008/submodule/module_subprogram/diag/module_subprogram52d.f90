!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram52d
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
!*  Define a module procedure with no function or subroutine declared
!*   in an interface body in the submodule or ancestor [sub]module
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
END MODULE m

SUBMODULE (m) n
CONTAINS
  ! for a function
  module procedure func2
    func2 = 2
  end procedure func2

  ! for a subroutine
  module procedure sub2
  end procedure sub2

END SUBMODULE n
