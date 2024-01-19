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
!*    The selector is a string, and redefine the associating entity
!*    (Exec wrong)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM ConstString
  IMPLICIT NONE

  CHARACTER(3) :: S = "123"

  ASSOCIATE ( As => S )
    IF ( As .NE. "123" ) ERROR STOP 50
    ASSOCIATE ( As1 => s )
      IF ( As1 .NE. "123" ) ERROR STOP 51
      As1 = "456"
      IF ( As1 .NE. "456" ) ERROR STOP 52
      IF ( As  .NE. "456" ) ERROR STOP 53
    END ASSOCIATE
    IF ( As  .NE. "456" ) ERROR STOP 54
  END ASSOCIATE

  END


