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
!*  Label on named associate construct
!* ( ICE: "*** PROGRAM ERROR *** No OTHERWISE or WHEN for execution in SELECT statement.")
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc11
  IMPLICIT NONE

1   A1:ASSOCIATE (As  => 1 )
5     IF ( As .NE. 1 ) ERROR STOP 11
9   END ASSOCIATE A1

  END
