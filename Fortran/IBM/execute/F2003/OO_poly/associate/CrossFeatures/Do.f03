! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 10, 2005
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
!*    The Do stmt
!*    (ICE-301000)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER   :: Id = 1
      CHARACTER :: C  ="1"
      LOGICAL   :: L  = .FALSE.
      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT)(A)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

    ELEMENTAL FUNCTION GetC(Arg)
    IMPLICIT CLASS(DT)(A)
    INTENT(IN) :: Arg
    CHARACTER  :: GetC
      GetC = Arg%C
    END FUNCTION

    ELEMENTAL FUNCTION GetL(Arg)
    IMPLICIT CLASS(DT)(A)
    INTENT(IN) :: Arg
    LOGICAL    :: GetL
      GetL = Arg%L
    END FUNCTION

  END MODULE

  PROGRAM Do

  USE M
  IMPLICIT TYPE(DT)(A)
  DIMENSION :: Arr(2:130)
  LOGICAL(8) :: LArr(2:130)
  INTEGER :: i


  ASSOCIATE ( As => (/(DT(Id=i), i=2, 130)/) )
  ASSOCIATE ( As => As%ID )
  ASSOCIATE ( As1 => Arr(2)%ID, As2 => As(129) )
    DO As1=2, As2
      Arr(As1)%L = .TRUE.
    END DO
  END ASSOCIATE
  END ASSOCIATE
  END ASSOCIATE

  IF ( Any(Arr%L      .NEQV. .TRUE.) ) ERROR STOP 60
  IF ( ANY(Arr%GetL() .NEQV. .TRUE.) ) ERROR STOP 61

  ASSOCIATE ( As => (/(DT(C=CHAR(i)), i=2, 130)/) )
  ASSOCIATE ( As => As%ID )
  ASSOCIATE ( As1 => Arr(2)%ID, As2 => As(129) )
    DO As1=2, As2
      Arr(As1)%L = .TRUE.
    END DO
  END ASSOCIATE
  END ASSOCIATE
  END ASSOCIATE

  IF ( Any(Arr%L      .NEQV. .TRUE.) ) ERROR STOP 70
  IF ( ANY(Arr%GetL() .NEQV. .TRUE.) ) ERROR STOP 71


  ASSOCIATE ( As => Arr )
  ASSOCIATE ( As => As%ID )
  ASSOCIATE ( As1 => As(129), As2 => 130 )
    DO As1=2, As2
      Arr(As1)%C = CHAR(As1)
    END DO
  END ASSOCIATE
  END ASSOCIATE
  END ASSOCIATE

  IF ( Any(Arr%C      .NE. (/(CHAR(i), i=2, 130)/)) ) ERROR STOP 80
  IF ( ANY(Arr%GetC() .NE. (/(CHAR(i), i=2, 130)/)) ) ERROR STOP 81

  END

