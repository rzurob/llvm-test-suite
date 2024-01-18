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
!*    The selector is a constant expression
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM ConstExpr
  IMPLICIT NONE

  CHARACTER(3) :: S = "123"

  ASSOCIATE ( As => S//"21" )
    IF ( As .NE. "12321" ) STOP 50
  END ASSOCIATE

  ASSOCIATE ( As => .TRUE. , As0 => .FALSE._2 )
    IF ( As .NEQV. .TRUE. ) STOP 52
    ASSOCIATE ( As1 => As .and. As0 )
      IF ( As1 .NEQV. .False. ) STOP 51
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => (1.0_8, 1.0_8) , As0 => (1.0, 1.0 ))
    IF ( As .NE. (1.0, 1.0) ) STOP 60
    ASSOCIATE ( As1 => As + As0 )
      IF ( As1 .NE. (2.0, 2.0) ) STOP 61
    END ASSOCIATE
    ASSOCIATE ( As => As + As )
      IF ( As .NE. (2.0, 2.0) ) STOP 61
    END ASSOCIATE
    IF ( As .NE. (1.0, 1.0) ) STOP 61
  END ASSOCIATE


  END


