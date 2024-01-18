! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 09, 2005
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
!*   The association
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc30
  IMPLICIT NONE

  INTEGER :: i=1

  ASSOCIATE ( A1 => i )

  ASSOCIATE ( A2 => A1+A1 )
    IF ( A1 .NE. 1 .OR. A2 .NE. 2 ) STOP 20
    A1 = 2
    IF ( A1 .NE. 2 .OR. A2 .NE. 2 ) STOP 21
  END ASSOCIATE

  ASSOCIATE ( A2 => A1+A1 )
    IF ( A1 .NE. 2 .OR. A2 .NE. 4 ) STOP 30
    i = 3
    IF ( A1 .NE. 3 .OR. A2 .NE. 4 ) STOP 31
  END ASSOCIATE

  ASSOCIATE ( A2 => A1 )
    IF ( A1 .NE. 3 .OR. A2 .NE. 3 ) STOP 40
    i = 4
    IF ( A1 .NE. 4 .OR. A2 .NE. 4 ) STOP 41
    A2 = 5
  END ASSOCIATE

  IF ( i .NE. 5 ) STOP 51

  END ASSOCIATE

  END


