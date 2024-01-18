! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     - The selector in select type construct is not poly
!*     (ICE)
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C810Misc2
  IMPLICIT NONE

  SELECT TYPE( As  => "1")
  END SELECT

  END

