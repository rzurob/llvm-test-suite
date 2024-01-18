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
!*  Incorrectly specify the order of the module, submodule in a
!*   submodule declaration, expecting a severe message at compile.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890323456789032345678903234567890323456789032345678903234567890

MODULE m
END MODULE m

SUBMODULE (m) b
END SUBMODULE b

! b:m incorrectly specified, should be flagged by compiler
SUBMODULE (b:m) c
END SUBMODULE c

PROGRAM submodule03d
USE m
END PROGRAM submodule03d
