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
!*  (ICE-300429)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Scope2
  IMPLICIT NONE
  INTEGER :: A = 5, B=1

  CALL Sub(A)
  IF ( A .NE. 3 ) ERROR STOP 22
  IF ( B .NE. 1 ) ERROR STOP 23

  CONTAINS

  SUBROUTINE Sub(B)
  CLASS(*) :: B

  SELECT TYPE (B)
  TYPE IS (INTEGER)

    ASSOCIATE ( A => B )
      IF ( A .NE. 5 ) ERROR STOP 11
      A = 3
      ASSOCIATE ( A => A )
        IF ( A .NE. 3 ) ERROR STOP 13
      END ASSOCIATE
    END ASSOCIATE

  CLASS DEFAULT
    STOP 33
  END SELECT

  END SUBROUTINE
  END