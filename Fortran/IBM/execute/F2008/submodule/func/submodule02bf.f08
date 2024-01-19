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
SUBMODULE (m) b
IMPLICIT NONE
CONTAINS
  module integer function func(x)
    integer, intent(in) :: x
    func = x * 2
  end function func
END SUBMODULE b
