! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 09, 2005
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
!*    The selector is an entity with implicit type
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM Implicit

  USE M
  IMPLICIT TYPE(DT)(A)

  ASSOCIATE( As => As )

    IF ( As%ID      .NE. 1 ) ERROR STOP 30
    IF ( As%GetID() .NE. 1 ) ERROR STOP 31

    As%ID = 2

    IF ( As%ID      .NE. 2 ) ERROR STOP 40
    IF ( As%GetID() .NE. 2 ) ERROR STOP 41

  END ASSOCIATE

  IF ( As%ID      .NE. 2 ) ERROR STOP 50
  IF ( As%GetID() .NE. 2 ) ERROR STOP 51

  END

