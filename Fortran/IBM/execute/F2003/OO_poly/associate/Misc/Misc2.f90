! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 02, 2004
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
!*    Selector is a statement function call
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Misc2
  IMPLICIT COMPLEX(C), CHARACTER(3)(S)
  CF(X) = (X, X)
  SF( ) = "123"

  ASSOCIATE ( As => CF(2.0) )
    IF (As .NE. (2.0, 2.0) ) STOP 50
  END ASSOCIATE

  ASSOCIATE ( As => SF() )
    IF (As .NE. "123" ) STOP 51
  END ASSOCIATE
  END
