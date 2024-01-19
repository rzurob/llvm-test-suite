! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 03, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Specification expression
!*  (299523)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc27
  TYPE :: DT
  END TYPE

  INTEGER :: V = 1

  TYPE :: Test
    LOGICAL :: W(SIZE((/V/)))
  END TYPE

  END





