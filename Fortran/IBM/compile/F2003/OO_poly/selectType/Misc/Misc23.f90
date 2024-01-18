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
!*  Pass on nonexist type name
!*  (299272/317690) -- the error msg is not good now(minor issue)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc23
  IMPLICIT TYPE(X)(U)
  U = X()
  END



