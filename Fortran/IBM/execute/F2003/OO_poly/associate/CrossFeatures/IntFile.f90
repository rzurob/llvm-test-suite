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
!*    The internal file
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER      :: Id = 1
      CHARACTER(3) :: C  = "1"
      LOGICAL      :: L  = .TRUE.

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

  PROGRAM IntFile

  USE M
  IMPLICIT TYPE(DT)(A)
  DIMENSION :: Arr(2:130)
  INTEGER :: i
  CHARACTER(3) :: C


  ASSOCIATE ( As => Arr )
    DO i=2, 129
      WRITE(As(i)%C, FMT=*) "!"
      READ(As(i)%C, FMT=*) As(i+1)%C
      IF ( TRIM(As(i+1)%C) .NE. "!" ) STOP 20
    END DO
  END ASSOCIATE

  IF ( Any(Arr%L      .NEQV. .TRUE.) ) STOP 60
  IF ( ANY(Arr%GetL() .NEQV. .TRUE.) ) STOP 61

  IF ( Any(Arr%ID      .NE. 1) ) STOP 60
  IF ( ANY(Arr%GetID() .NE. 1) ) STOP 61

  END

