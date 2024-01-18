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
!*   The generic interface
!*    ()
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

  PROGRAM InterfaceGen

  USE M
  IMPLICIT TYPE(DT)(F)

  INTERFACE Fun
    FUNCTION Fun1()
    IMPORT DT
    IMPLICIT TYPE(DT)(F)
    END FUNCTION

    FUNCTION Fun2(Arg)
    IMPORT DT
    TYPE(DT) :: Fun2, Arg
    END FUNCTION
  END INTERFACE

  ASSOCIATE ( As => Fun(Fun()) )

    IF ( As%ID       .NE. -1 ) STOP 20
    IF ( As%GetID()  .NE. -1 ) STOP 21

    IF ( As%C       .NE. "!" ) STOP 30
    IF ( As%GetC()  .NE. "!" ) STOP 31

    IF ( As%L       .NEQV. .TRUE. ) STOP 60
    IF ( As%GetL()  .NEQV. .TRUE. ) STOP 61


  END ASSOCIATE


  END

  FUNCTION Fun1()
  USE M
  TYPE(DT) :: Fun1
    Fun1 = DT(ID=-1, C="!", L=.TRUE.)
  END FUNCTION

  FUNCTION Fun2(Arg)
  USE M
  TYPE(DT) :: Fun2,Arg
    Fun2 = Arg
  END FUNCTION

