!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule02f
!*
!*  DATE                       : 6 December, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Declare a function in a module and define it in a submodule, in
!*   separate files.  Verify the creation of .mod and .smod files
!*   with the correct naming convention.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890223456789022345678902234567890223456789022345678902234567890
PROGRAM submodule02f
USE m
IMPLICIT NONE
integer :: a = 2
print *, "a =", a, ", 2a=", func(a)
END PROGRAM submodule02f
