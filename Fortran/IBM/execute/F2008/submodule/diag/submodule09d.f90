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
!*    Call a function declared in the host interface but not defined
!*  in any descendant submodule expecting a severe message at compile
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
END INTERFACE
END MODULE m

SUBMODULE (m) n
END SUBMODULE n

PROGRAM submodule09d
USE m
!  implicit none
real :: a
a = func()
END PROGRAM submodule09d
