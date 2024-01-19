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
!*  (299886)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc28

  CALL Sub((/1,2,3/), 3)

  CONTAINS
  SUBROUTINE Sub(Arr, N)
  INTEGER ::  N
  CLASS(*) :: Arr(*)
    SELECT TYPE ( As => Arr(1:N) )
    TYPE IS (INTEGER)
      IF (ANY(SHAPE(As) .NE. (/3/))) ERROR STOP 30
    END SELECT
  END SUBROUTINE
  END


