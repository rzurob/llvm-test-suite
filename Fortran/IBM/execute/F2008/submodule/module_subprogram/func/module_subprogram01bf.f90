!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram01f
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
!*  Declare a subroutine in a module and define it in a submodule
!*   in separate files.  Verify the creation of .mod and .smod files
!*   with the correct naming convention.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM module_subprogram01f
USE m
IMPLICIT NONE
integer :: a = 2, aa

call sub(a, aa)
if (aa .ne.4) error stop 66

END PROGRAM module_subprogram01f
