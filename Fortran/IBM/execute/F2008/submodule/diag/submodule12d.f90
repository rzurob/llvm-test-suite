!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule12d
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
!*    Call a function declared in the host interface defined differently
!*  in a descendant submodule expecting severe message at compile time
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890923456789092345678909234567890923456789092345678909234567890
MODULE m
implicit none
INTERFACE
  module function func()
    real :: func
  end function

  module function func2(arg1)
    real func2
    integer :: arg1
  end function

END INTERFACE
END MODULE m

SUBMODULE (m) n
CONTAINS
  module function func()
    integer func
    func = 0
  end function

  module function func2(arg1)
    real func2
    real arg1
    func2 = arg1
  end function

END SUBMODULE n

PROGRAM submodule12d
USE m
implicit none
real :: a
a = func()
a = func2(a)
END PROGRAM submodule12d
