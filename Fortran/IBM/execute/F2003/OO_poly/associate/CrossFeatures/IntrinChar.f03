! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 07, 2005
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
!*    The selector is a character literal of default
!*    (ICE-300823)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM IntrinChar
  IMPLICIT NONE
  INTEGER :: K
  PARAMETER (K=1)

  ASSOCIATE ( As => 1_"1234567890" )
    IF ( As  .NE. 1_"1234567890" )        ERROR STOP 20
    ASSOCIATE ( As => As )
      IF ( As       .NE. 1_"1234567890" ) ERROR STOP 21
      IF ( KIND(As) .NE. 1 )              ERROR STOP 22
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => K_"1234567890" )
    IF ( As    .NE. K_"1234567890" )      ERROR STOP 30
    ASSOCIATE ( As => As )
      IF ( As       .NE. K_"1234567890" ) ERROR STOP 31
      IF ( KIND(As) .NE. 1 )              ERROR STOP 32
    END ASSOCIATE
    CALL Sub(As)
  END ASSOCIATE


  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg

  SELECT TYPE(Arg)
  TYPE IS (CHARACTER(*))
    ASSOCIATE ( Arg=>Arg)
      IF ( Arg       .NE. K_"1234567890" ) ERROR STOP 41
      IF ( KIND(Arg) .NE. 1 )              ERROR STOP 42
    END ASSOCIATE
  CLASS DEFAULT
    STOP 44
  END SELECT

  END SUBROUTINE

  END
