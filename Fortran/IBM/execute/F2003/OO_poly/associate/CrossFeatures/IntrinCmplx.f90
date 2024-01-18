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
!*    The selector is of complex with various kinds
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM IntrinCmplx
  IMPLICIT NONE

  ASSOCIATE ( As => (4.0_4, -4.0_4) )
    ASSOCIATE ( As => Fun(As) )
      IF ( As       .NE. (4.0_4, -4.0_4) ) ERROR STOP 20
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => (8.0_8, -8.0_8) )
    ASSOCIATE ( As => Fun(As) )
      IF ( As       .NE. (8.0_8, -8.0_8) ) ERROR STOP 22
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => (16.0_16, -16.0_16) )
    ASSOCIATE ( As => Fun(As) )
      IF ( As       .NE. (16.0_16, -16.0_16) ) ERROR STOP 24
    END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*) :: Arg
  COMPLEX(4), POINTER  :: Fun

    ASSOCIATE ( Arg => Arg)
    SELECT TYPE (Arg)
    CLASS DEFAULT
      STOP 99
    TYPE IS (COMPLEX)
      ALLOCATE (Fun, SOURCE=CMPLX(Arg, KIND=4))
    TYPE IS (COMPLEX(8))
      ALLOCATE (Fun, SOURCE=CMPLX(Arg, KIND=4))
    TYPE IS (COMPLEX(16))
      ALLOCATE (Fun, SOURCE=CMPLX(Arg, KIND=4))
    END SELECT
    END ASSOCIATE

  END FUNCTION

  END

