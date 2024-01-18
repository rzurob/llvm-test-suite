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
!*    The selector is a special char
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SpecChar
  IMPLICIT NONE

  ASSOCIATE ( As => "\b" )
  ASSOCIATE ( As => "\n" )
  ASSOCIATE ( As => "\t" )
  ASSOCIATE ( As => "\'" )
  ASSOCIATE ( As => "\"" )
  ASSOCIATE ( As => "\\" )
  ASSOCIATE ( As => "\x" )
    IF ( As .NE. "\x" ) ERROR STOP 26
  END ASSOCIATE
    IF ( As .NE. "\\" ) ERROR STOP 25
  END ASSOCIATE
    IF ( As .NE. "\"" ) ERROR STOP 24
  END ASSOCIATE
    IF ( As .NE. "\'" ) ERROR STOP 23
  END ASSOCIATE
    IF ( As .NE. "\t" ) ERROR STOP 22
  END ASSOCIATE
    IF ( As .NE. "\n" ) ERROR STOP 21
  END ASSOCIATE
    IF ( As .NE. "\b" ) ERROR STOP 20
  END ASSOCIATE

  ASSOCIATE ( As0 => "\x", As1 => "\n", As2 => "\"" )
    IF ( As0 // As1 // As2 .NE. "\x" // "\n" // "\"" ) ERROR STOP 27
  END ASSOCIATE

  END
