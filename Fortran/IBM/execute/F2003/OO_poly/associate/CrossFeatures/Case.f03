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
!*    The Case stmt
!*    ()
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

  PROGRAM Case

  USE M
  IMPLICIT TYPE(DT)(A)
  DIMENSION :: Arr(2:130)
  LOGICAL(8) :: LArr(2:130)
  INTEGER :: i


  ASSOCIATE ( As => (/(DT(Id=i), i=2, 130)/) )
    SELECT CASE (As(1)%ID)
    CASE (:-1)
      STOP 20
    CASE (0)
      STOP 21
    CASE (1:)
      Arr  = As
    END SELECT
  END ASSOCIATE

  IF ( Any(Arr(:)%ID      .NE. (/(i, i=2,130)/)) ) ERROR STOP 60
  IF ( ANY(Arr(:)%GetID() .NE. (/(i, i=2,130)/)) ) ERROR STOP 61

  ASSOCIATE ( As => Arr )
    SELECT CASE (As(3)%C)
    CASE (:CHAR(48))
      STOP 30
    CASE (CHAR(50):)
      STOP 31
    CASE (CHAR(49))
      As%C = (/(CHAR(4+i), i=2, 130)/)
    END SELECT
  END ASSOCIATE

  IF ( Any(Arr(:)%C      .NE. (/(CHAR(4+i), i=2, 130)/)) ) ERROR STOP 70
  IF ( ANY(Arr(:)%GetC() .NE. (/(CHAR(4+i), i=2, 130)/)) ) ERROR STOP 71

  ASSOCIATE ( As => Arr(::2) )
    SELECT CASE (As(3)%L)
    CASE (.TRUE.)
      STOP 40
    CASE (.FALSE.)
      As%L = .TRUE.
    END SELECT
  END ASSOCIATE

  IF ( Any(Arr(::2)%L      .NEQV. .TRUE.) ) ERROR STOP 80
  IF ( ANY(Arr(::2)%GetL() .NEQV. .TRUE.) ) ERROR STOP 81

  IF ( Any(Arr(3::2)%L      .NEQV. .FALSE.) ) ERROR STOP 83
  IF ( ANY(Arr(3::2)%GetL() .NEQV. .FALSE.) ) ERROR STOP 84

  END

