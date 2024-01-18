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
!*   The procedure stmt
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER      :: Id = 0
      CHARACTER(3) :: C  = " "
      LOGICAL      :: L  = .FALSE.

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

  PROGRAM ProcedureStmt

  USE M
  IMPLICIT TYPE(DT)(F)

  TYPE(DT) :: V =  DT(ID=-1, C="!", L=.TRUE.)

  INTERFACE
    FUNCTION Func1(Arg)
    IMPORT DT
    IMPLICIT TYPE(DT)(F)
    INTEGER, INTENT(IN) :: Arg
    END FUNCTION

    FUNCTION Func2(Arg)
    IMPORT DT
    TYPE(DT), INTENT(IN) :: Arg
    TYPE(DT)             :: Func2
    END FUNCTION
  END INTERFACE

  PROCEDURE(Func1) :: Fun1
  PROCEDURE(Func2) :: Fun2

  CALL Sub(Fun2)

  CONTAINS

  SUBROUTINE Sub(Arg)
  PROCEDURE(Fun2) :: Arg

  ASSOCIATE ( As => Arg(V) )

    IF ( As%ID       .NE. -1 ) ERROR STOP 20
    IF ( As%GetID()  .NE. -1 ) ERROR STOP 21

    IF ( As%C       .NE. "!" ) ERROR STOP 30
    IF ( As%GetC()  .NE. "!" ) ERROR STOP 31

    IF ( As%L       .NEQV. .TRUE. ) ERROR STOP 60
    IF ( As%GetL()  .NEQV. .TRUE. ) ERROR STOP 61


  END ASSOCIATE

  END SUBROUTINE

  END

  FUNCTION Fun1(Arg)
  USE M
  TYPE(DT) :: Fun1
  INTEGER, INTENT(IN)  :: Arg
    Fun1 = DT(ID=-4, C="4", L=.TRUE.)
  END FUNCTION

  FUNCTION Fun2(Arg)
  USE M
  TYPE(DT), INTENT(IN) :: Arg
  TYPE(DT) :: Fun2
    Fun2 = Arg
  END FUNCTION

