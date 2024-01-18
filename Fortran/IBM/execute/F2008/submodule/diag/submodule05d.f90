!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule05d
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
!*  Set up naming conflict between descendant submodule b of the host
!*   module and another descendant submodule expecting a severe message
!*   at compile.
!*
!*  SUBMODULE (m:b) b
!*  END SUBMODULE b
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890423456789042345678904234567890423456789042345678904234567890

MODULE m
END MODULE m

SUBMODULE (m) b
END SUBMODULE b

SUBMODULE (m:b) b
END SUBMODULE b

PROGRAM submodule05d
USE m
END PROGRAM submodule05d
