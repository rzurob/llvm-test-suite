!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule10d
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
!*    Call a subroutine declared in the host interface but not defined
!*  in any descendant submodule expecting a link error
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567891023456789102345678910234567891023456789102345678910234567890
MODULE m
implicit none
INTERFACE
  module subroutine sub()
  end subroutine
END INTERFACE
END MODULE m

SUBMODULE (m) n
END SUBMODULE n

PROGRAM submodule10d
USE m
implicit none
call sub()
END PROGRAM submodule10d
