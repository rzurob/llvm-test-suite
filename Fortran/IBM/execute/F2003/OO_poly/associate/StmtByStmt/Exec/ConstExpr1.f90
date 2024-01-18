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
!*    The selector is a constant expression with user defined operations
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM ConstExpr1
  IMPLICIT NONE

  INTERFACE OPERATOR ( * )
    FUNCTION BOOLEAN_AND (Arg1, Arg2)
      LOGICAL, INTENT (IN) :: Arg1, Arg2
      LOGICAL              :: BOOLEAN_AND
    END FUNCTION BOOLEAN_AND
  END INTERFACE OPERATOR ( * )

  ASSOCIATE ( As => .TRUE. * .TRUE. , As0 => .TRUE. * .FALSE. )
    IF ( As .NEQV. .TRUE. ) STOP 52
    ASSOCIATE ( As1 => As .and. As0 )
      IF ( As1 .NEQV. .False. ) STOP 51
    END ASSOCIATE
  END ASSOCIATE


  END

  FUNCTION BOOLEAN_AND (Arg1, Arg2)
    LOGICAL, INTENT (IN) :: Arg1, Arg2
    LOGICAL              :: BOOLEAN_AND
    BOOLEAN_AND = Arg1 .AND. Arg2
  END FUNCTION BOOLEAN_AND


