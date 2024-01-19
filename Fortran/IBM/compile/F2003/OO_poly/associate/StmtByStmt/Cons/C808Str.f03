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
!*    The selector is an structure constructor with private componet
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE T
      INTEGER, PRIVATE :: P = 1
      INTEGER          :: Q
    END TYPE

    TYPE(T), SAVE :: Var

  END MODULE

  PROGRAM C808Arr
  USE M
  IMPLICIT NONE

  TYPE, EXTENDS(T) :: DT
  END TYPE

  TYPE(DT) :: V

    ASSOCIATE ( As => Var%P )
      As%P = 1
    END ASSOCIATE

    ASSOCIATE ( As => DT(Q=1) )
      As%P = 1
    END ASSOCIATE

    ASSOCIATE ( As => DT(P=1, Q=2) )
      As%P = 1
    END ASSOCIATE

  END
