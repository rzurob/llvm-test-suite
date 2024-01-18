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
!*    The selector is a dummy entity with implicit type
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
    IMPLICIT CLASS(DT)(A,B)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM ImplicitDummy

  USE M
  IMPLICIT TYPE(DT)(A), CLASS(*)(B)

  CALL Sub(A)
  IF ( A%ID      .NE. 2 ) ERROR STOP 60
  IF ( A%GetID() .NE. 2 ) ERROR STOP 61

  CONTAINS

  SUBROUTINE Sub(B)

  ASSOCIATE( As => B )
  SELECT TYPE( As )
  CLASS IS (DT)

    IF ( As%ID      .NE. 1 ) ERROR STOP 30
    IF ( As%GetID() .NE. 1 ) ERROR STOP 31

    As%ID = 2

    IF ( As%ID      .NE. 2 ) ERROR STOP 40
    IF ( As%GetID() .NE. 2 ) ERROR STOP 41

  CLASS DEFAULT
    STOP 99
  END SELECT
  END ASSOCIATE

  END SUBROUTINE

  END


