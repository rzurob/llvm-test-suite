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
!*  "C810DiffName1.f", line 50.27: 1511-060 (E) Construct name on ASSOCIATE
!*  statement does not match with the one specified on the previous SELECT CASE
!*  statement.  Statement will be matched with the previous SELECT CASE statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C810DiffName1
  IMPLICIT NONE

   test1:   ASSOCIATE ( As  => "abc"(1:3) )
              IF ( As .NE. "bc" ) STOP 50
            END ASSOCIATE test2

  END

