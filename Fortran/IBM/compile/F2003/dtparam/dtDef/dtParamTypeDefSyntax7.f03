!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 29, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax
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
!*  syntax of derived type stmt
!*  wrong usage
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefSyntax7

  PARAMETER (KC=4)

  TYPE DT
  END TYPE

  TYPE, ABSTRACT :: DT2(K)
  END TYPE

  TYPE :: DT4()
  END TYPE

  TYPE, EXTENDS() :: DT4(K)
  END TYPE

  END
