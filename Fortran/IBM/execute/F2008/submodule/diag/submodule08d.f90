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
!*  Access a variable defined in a submodule expecting a sever message
!*   at compile time
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890823456789082345678908234567890823456789082345678908234567890
MODULE m
implicit none
END MODULE m

SUBMODULE (m) n
integer :: a = 7
END SUBMODULE n

PROGRAM submodule08d
implicit none
USE m
print *, "a=", a
END PROGRAM submodule08d
