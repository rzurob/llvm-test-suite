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
!*    The selector is of int type with various kinds
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM IntrinInt
  IMPLICIT NONE

  ASSOCIATE ( As => 1_1 )
    ASSOCIATE ( As => Fun(As) )
      IF ( As       .NE. 1_1 ) STOP 20
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => 2_2 )
    ASSOCIATE ( As => Fun(As) )
      IF ( As       .NE. 2_2 ) STOP 22
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => 4_4 )
    ASSOCIATE ( As => Fun(As) )
      IF ( As       .NE. 4_4 ) STOP 24
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => 8_8 )
    ASSOCIATE ( As => Fun(As) )
      IF ( As       .NE. 8_8 ) STOP 28
    END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*) :: Arg
  INTEGER, ALLOCATABLE  :: Fun

    ASSOCIATE ( Arg => Arg)
    SELECT TYPE (Arg)
    CLASS DEFAULT
      STOP 99
    TYPE IS (INTEGER(1))
      ALLOCATE (Fun, SOURCE=INT(Arg, 4))
    TYPE IS (INTEGER(2))
      ALLOCATE (Fun, SOURCE=INT(Arg, 4))
    TYPE IS (INTEGER(4))
      ALLOCATE (Fun, SOURCE=INT(Arg, 4))
    TYPE IS (INTEGER(8))
      ALLOCATE (Fun, SOURCE=INT(Arg, 4))
    END SELECT
    END ASSOCIATE

  END FUNCTION

  END

