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
!*  Check on definablity
!*  (299345)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc24

  CALL Sub("123 ")

  CONTAINS

  SUBROUTINE Sub( V)
  CLASS(*), INTENT(IN)  :: V

  SELECT TYPE (V)
  TYPE IS (CHARACTER(*))
    V = "Bad!"
    PRINT *, V
  END SELECT

  END SUBROUTINE

  END




