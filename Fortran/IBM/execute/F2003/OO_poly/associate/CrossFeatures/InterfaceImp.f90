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
!*   The implicit interface
!*    (ICE-301017)
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

  PROGRAM InterfaceImp

  USE M
  IMPLICIT TYPE(DT)(F)


  ASSOCIATE ( As => (/ Fun(DT(ID=-1, C="111", L=.TRUE.)), &
            &          Fun(DT(ID=-2, C="222", L=.TRUE.)), &
            &          Fun(DT(ID=-3, C="333", L=.TRUE.))  /) )

    IF ( Any(As%ID      .NE. (/-1,-2,-3/)) ) STOP 20
    IF ( ANY(As%GetID() .NE. (/-1,-2,-3/)) ) STOP 21

    IF ( Any(As%C      .NE. (/"111","222","333"/)) ) STOP 30
    IF ( ANY(As%GetC() .NE. (/"1","2","3"/)) )       STOP 31

    IF ( Any(As%L      .NEQV. .TRUE.) ) STOP 60
    IF ( ANY(As%GetL() .NEQV. .TRUE.) ) STOP 61


  END ASSOCIATE


  END

  FUNCTION Fun(Arg)
  USE M
  TYPE(DT) :: Fun,Arg
    Fun = Arg
  END FUNCTION


