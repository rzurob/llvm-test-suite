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
!*  The parent-submodule-name is not the name of the parent submodule
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

MODULE a
END MODULE

SUBMODULE (a) b
END SUBMODULE

SUBMODULE (m) n
END SUBMODULE

SUBMODULE (m:n) o
END SUBMODULE

SUBMODULE (m:b) p
END SUBMODULE

PROGRAM submodule16d
USE m
END PROGRAM submodule16d
