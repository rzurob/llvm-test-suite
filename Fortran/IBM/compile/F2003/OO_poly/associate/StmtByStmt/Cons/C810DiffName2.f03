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
!*
!*  Wrong diag msg:
!*   "C810DiffName2.f", line 55.20: 1511-059 (E) ASSOCIATE statement must not
!*   contain a construct name.  Statement will be matched with the previous
!*  SELECT CASE statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C810DiffName2
  IMPLICIT NONE

     ASSOCIATE ( As  => "abc"(1:3) )
       IF ( As .NE. "bc" ) ERROR STOP 50
     END ASSOCIATE As

  END

