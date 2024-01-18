!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule18d
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
!*    The specification-part contains a format-stmt, entry-stmt, or
!*  stmt-function-stmt
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890423456789042345678904234567890423456789042345678904234567890
MODULE m
END MODULE

SUBMODULE (m) n

100 format (A, F3.2)
ENTRY enterhere()
C(F) = 5.0*(F - 32.0)/9.0

END SUBMODULE
