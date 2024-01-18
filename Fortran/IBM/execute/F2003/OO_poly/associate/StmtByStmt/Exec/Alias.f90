! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 01, 2005
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
!*    The associate name is reused many times
!*   ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Alias
  IMPLICIT NONE

  ASSOCIATE ( As => 1 )
  ASSOCIATE ( As => 2 )
  ASSOCIATE ( As => 3 )
  ASSOCIATE ( As => 4 )
  ASSOCIATE ( As => 5 )
  ASSOCIATE ( As => 6 )
  ASSOCIATE ( As => 7 )
  ASSOCIATE ( As => 8 )
  ASSOCIATE ( As => 9 )
  ASSOCIATE ( As => 0 )
    IF ( As .NE. 0 ) ERROR STOP 20
  END ASSOCIATE
    IF ( As .NE. 9 ) ERROR STOP 19
  END ASSOCIATE
    IF ( As .NE. 8 ) ERROR STOP 18
  END ASSOCIATE
    IF ( As .NE. 7 ) ERROR STOP 17
  END ASSOCIATE
    IF ( As .NE. 6 ) ERROR STOP 16
  END ASSOCIATE
    IF ( As .NE. 5 ) ERROR STOP 15
  END ASSOCIATE
    IF ( As .NE. 4 ) ERROR STOP 14
  END ASSOCIATE
    IF ( As .NE. 3 ) ERROR STOP 13
  END ASSOCIATE
    IF ( As .NE. 2 ) ERROR STOP 12
  END ASSOCIATE
    IF ( As .NE. 1 ) ERROR STOP 11
  END ASSOCIATE

  END

