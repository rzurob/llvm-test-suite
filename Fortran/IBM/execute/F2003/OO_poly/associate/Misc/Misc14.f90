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
!*  Nested associate constructs
!* (ICE : 294652 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc14

    ASSOCIATE ( As => (/1,2,3/))
      ASSOCIATE (As0 => As)
      END ASSOCIATE
    END ASSOCIATE

  END

