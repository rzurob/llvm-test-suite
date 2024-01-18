! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate construct name
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
!*     C810 : The associate construct shall have the same name
!*     - Different names
!*    (Pass comp-the msg is :
!*    1516-246 (E) The construct name for the END ASSOCIATE statement is missing.
!*    The statement will be matched with the previous ASSOCIATE statement)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C810DiffName
  IMPLICIT NONE

 As:   ASSOCIATE ( As  => "abc"(1:3) )
      IF ( As .NE. "abc" ) STOP 50
    END ASSOCIATE

  END

