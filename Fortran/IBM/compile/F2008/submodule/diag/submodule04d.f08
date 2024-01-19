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
!*  Set up naming conflict between the host module and a submodule
!*   expecting a  severe message at compile.
!*
!*  SUBMODULE (m:b) m
!*  END SUBMODULE m
!*
!* ===================================================================
!234567890423456789042345678904234567890423456789042345678904234567890

PROGRAM submodule04d
USE m
END PROGRAM submodule04d
