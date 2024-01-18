! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 25, 2005
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
!*  Scope
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    COMPLEX(8) :: A(3:6) = (-1.0_8, 1.0_8), B(4)=(1.0_8, -1.0_8)
  END MODULE

  PROGRAM Scope
  USE M
  IMPLICIT NONE

  ASSOCIATE ( A => B )
    IF ( ANY(LBOUND(A) .NE. (/1/) ) )          STOP 11
    IF ( ANY(SHAPE(A)  .NE. (/4/) ) )          STOP 12
    IF ( ANY(A         .NE. (1.0_8,-1.0_8) ) ) STOP 13

    B = (2.0_8, -2.0_8)
    IF ( ANY(LBOUND(A) .NE. (/1/) ) )          STOP 41
    IF ( ANY(SHAPE(A)  .NE. (/4/) ) )          STOP 42
    IF ( ANY(A         .NE. (2.0_8,-2.0_8) ) ) STOP 43

  END ASSOCIATE

  IF ( ANY(LBOUND(A) .NE. (/3/) ) )          STOP 51
  IF ( ANY(SHAPE(A)  .NE. (/4/) ) )          STOP 52
  IF ( ANY(A         .NE. (-1.0_8,1.0_8) ) ) STOP 53

  IF ( ANY(LBOUND(B) .NE. (/1/) ) )          STOP 61
  IF ( ANY(SHAPE(B)  .NE. (/4/) ) )          STOP 62
  IF ( ANY(B         .NE. (2.0_8,-2.0_8) ) ) STOP 63

  END
