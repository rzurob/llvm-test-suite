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

  PROGRAM Scope
  IMPLICIT NONE
  INTEGER :: A = 5, B=1

  ASSOCIATE ( A => B )
    IF ( A .NE. 1 ) STOP 11
    B = 3
    ASSOCIATE ( A => A )
      IF ( A .NE. 3 ) STOP 13
    END ASSOCIATE
  END ASSOCIATE
  IF ( A .NE. 5 ) STOP 12

  END
