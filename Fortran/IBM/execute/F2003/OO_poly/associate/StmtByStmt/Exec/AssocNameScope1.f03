! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 04, 2005
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
!*
!*  The associate name's scope
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameScopei1
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: V

  ALLOCATE(V, SOURCE=1_1)

  ASSOCIATE ( A => V)
    ASSOCIATE ( A => A )

      SELECT TYPE ( A )
      CLASS DEFAULT
        STOP 10
      TYPE IS ( INTEGER(1) )
        A = 2_1
      END SELECT

      SELECT TYPE ( V )
      CLASS DEFAULT
        STOP 10
      TYPE IS ( INTEGER(1) )
        IF ( V .NE. 2_1 ) ERROR STOP 11
        V = 3_1
      END SELECT

    END ASSOCIATE

    SELECT TYPE ( A )
    CLASS DEFAULT
      STOP 10
    TYPE IS ( INTEGER(1) )
      IF ( A .NE. 3_1 ) ERROR STOP 12
      A = 4_1
    END SELECT

  END ASSOCIATE

  SELECT TYPE ( V )
  CLASS DEFAULT
    STOP 10
  TYPE IS ( INTEGER(1) )
    IF ( V .NE. 4_1 ) ERROR STOP 13
  END SELECT


  END
