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
!*    The selector is a constant string
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM ConstString
  IMPLICIT NONE

  CHARACTER(10), PARAMETER :: S = "1234567890"
  CHARACTER(10), PARAMETER :: S2 = S(2:8)
  CHARACTER,     PARAMETER :: S3(6) = (/ "1", "2", "3", "4", "5", "6" /)


    ASSOCIATE ( As => S )
      IF ( As(1:10) .NE. "1234567890") ERROR STOP 50
      ASSOCIATE ( As0 => As(1:3), As => "123" )
         IF ( As0 .NE. "123") ERROR STOP 51
         IF ( As  .NE. "123") ERROR STOP 52
      END ASSOCIATE
      IF ( As  .NE. "1234567890") ERROR STOP 53
    END ASSOCIATE

    ASSOCIATE ( As => "1234567890" )
      IF ( As(1:10) .NE. "1234567890") ERROR STOP 55
      ASSOCIATE ( As0 => As(1:3), As => "123" )
         IF ( As0 .NE. "123") ERROR STOP 56
         IF ( As  .NE. "123") ERROR STOP 57
      END ASSOCIATE
      IF ( As  .NE. "1234567890") ERROR STOP 58
    END ASSOCIATE

    ASSOCIATE ( As => S(:) )
      IF ( As(1:10) .NE. "1234567890") ERROR STOP 60
      ASSOCIATE ( As0 => As(1:3), As => "123" )
         IF ( As0 .NE. "123") ERROR STOP 61
         IF ( As  .NE. "123") ERROR STOP 62
      END ASSOCIATE
      IF ( As  .NE. "1234567890") ERROR STOP 63
    END ASSOCIATE

    ASSOCIATE ( As => S3(1:6:2) )  ! Error here
      IF ( ANY(As .NE. (/"1", "3", "5"/))) ERROR STOP 70
      ASSOCIATE ( As0 => As(1:3) )
        IF ( ANY(As0 .NE. (/"1", "3", "5"/))) ERROR STOP 71
      END ASSOCIATE
      IF ( ANY (S3(2:6:2) .NE. (/"2", "4", "6"/))) ERROR STOP 72
    END ASSOCIATE
  END