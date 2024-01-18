!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC504d
!*
!*  DATE                       : May. 08, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* C504 (R504) If a type-param-value in a char-length in an entity-decl is not a colon or
!* an asterisk, it shall be a specification-expr.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC504d

  INTEGER   :: L
  CHARACTER :: C*(L )

  CONTAINS

  FUNCTION F()
  INTEGER   :: L1
  CHARACTER :: F*(L1)
  F = ""
  END FUNCTION

  END

