! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  DoWhile.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : DoWhile
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
!*    The DoWhile stmt
!*    (ICE)
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

  PROGRAM DoWhile

  USE M
  IMPLICIT TYPE(DT)(A)
  DIMENSION :: Arr(2:130)
  LOGICAL(8) :: LArr(2:130)
  INTEGER :: i


  ASSOCIATE ( As => (/(DT(Id=i), i=2, 130)/) )
  ASSOCIATE ( As => As%ID )
  ASSOCIATE ( As1 => Arr(2)%ID, As2 => As(129) )
    As1 = 2
    DO WHILE ( As1 .LE. As2 )
      Arr(As1)%L = .TRUE.
      As1 = As1 + 1
    END DO
  END ASSOCIATE
  END ASSOCIATE
  END ASSOCIATE

  IF ( Any(Arr%L      .NEQV. .TRUE.) ) STOP 60
  IF ( ANY(Arr%GetL() .NEQV. .TRUE.) ) STOP 61

  ASSOCIATE ( As => (/(DT(C=CHAR(i)), i=2, 130)/) )
  ASSOCIATE ( As => As%ID )
  ASSOCIATE ( As1 => Arr(2)%ID, As2 => As(129) )
    As1 = 2
    DO WHILE ( As1 .LE. As2 )
      Arr(As1)%L = .TRUE.
      As1 = As1 + 1
    END DO
  END ASSOCIATE
  END ASSOCIATE
  END ASSOCIATE

  IF ( Any(Arr%L      .NEQV. .TRUE.) ) STOP 70
  IF ( ANY(Arr%GetL() .NEQV. .TRUE.) ) STOP 71


  ASSOCIATE ( As => Arr )
  ASSOCIATE ( As => As%ID )
  ASSOCIATE ( As1 => As(129), As2 => 130 )
    As1 = 2
    DO WHILE ( As1 .LE. As2 )
      Arr(As1)%C = CHAR(As1)
      As1 = As1 + 1
    END DO
  END ASSOCIATE
  END ASSOCIATE
  END ASSOCIATE

  IF ( Any(Arr%C      .NE. (/(CHAR(i), i=2, 130)/)) ) STOP 80
  IF ( ANY(Arr%GetC() .NE. (/(CHAR(i), i=2, 130)/)) ) STOP 81

  END


