! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Selector is a constant
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
!*    The selector is an associate name which is a function call
!*   (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C808Func
  IMPLICIT NONE
  CHARACTER :: C = "3"

    ASSOCIATE ( As => Func())
       ASSOCIATE (As0 => As)
        As0 = "too bad!"
      END ASSOCIATE
    END ASSOCIATE

    ASSOCIATE ( As => C // Func() )
      ASSOCIATE ( As0 => As)
        READ(C, FMT="(A)") As0
      END ASSOCIATE
    END ASSOCIATE

    ASSOCIATE ( As => Func1() )
      ASSOCIATE ( As0 => As )
        DO As0 =1, 2
        END DO
      END ASSOCIATE
    END ASSOCIATE

    CONTAINS

    FUNCTION Func()
      CHARACTER(10) :: Func
      Func = "ok"
    END FUNCTION

    FUNCTION Func1()
      INTEGER :: Func1(3)
      Func1 = 1
    END FUNCTION


  END

