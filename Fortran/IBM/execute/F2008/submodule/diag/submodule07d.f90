!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule07d
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
!*    Use-associate a submodule expecting a severe message at compile
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890723456789072345678907234567890723456789072345678907234567890
MODULE m
END MODULE m

SUBMODULE (m) n
END SUBMODULE n

PROGRAM submodule07d
USE n
END PROGRAM submodule07d
