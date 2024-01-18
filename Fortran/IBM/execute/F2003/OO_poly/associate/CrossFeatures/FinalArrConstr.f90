! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  FinalArrConstr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FinalArrConstr
!*
!*  DATE                       : Mar. 14, 2005
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
!*   The finalization-Array  constructor (Interp-301240)
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTEGER :: INDEX=0
    INTEGER :: Fin(6)=0

    TYPE :: Base
      CONTAINS
      FINAL :: FinalBase
    END TYPE

    TYPE, EXTENDS(Base) :: DT
      INTEGER      :: Id = 0
      CHARACTER(3) :: C  = " "
      LOGICAL      :: L  = .FALSE.

      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL

      FINAL :: Final
      FINAL :: FinalArr

    END TYPE

  CONTAINS

    SUBROUTINE FinalBase(Obj)
    TYPE(Base) :: Obj
      Index = Index + 1
      Fin(Index) = 1
      PRINT *, "Base Finalization"
    END SUBROUTINE

    SUBROUTINE Final(Obj)
    TYPE(DT) :: Obj
      Index = Index + 1
      Fin(Index) = 2
      PRINT *, "DT Finalization"
    END SUBROUTINE

    SUBROUTINE FinalArr(ObjArr)
    TYPE(DT) :: ObjArr(:)
      Index = Index + 1
      Fin(Index) = 3
      PRINT *, "DT ARR Finalization"
    END SUBROUTINE

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

  PROGRAM FinalArrConstr

  USE M
  IMPLICIT TYPE(DT)(F)

  TYPE(DT), TARGET :: V =  DT(ID=-1, C="!", L=.TRUE.)

  Fin = -1

  ASSOCIATE ( As => (/Fun(V),Fun(V),Fun(V)/) )

    IF ( ANY(SHAPE(As) .NE. (/3/)) ) STOP 30

    IF ( ANY(As%ID       .NE. -1 )) STOP 20
    IF ( ANY(As%GetID()  .NE. -1 )) STOP 21

    IF ( ANY(As%C       .NE. "!" )) STOP 30
    IF ( ANY(As%GetC()  .NE. "!" )) STOP 31

    IF ( ANY(As%L       .NEQV. .TRUE. )) STOP 60
    IF ( ANY(As%GetL()  .NEQV. .TRUE. )) STOP 61

    INDEX = 1
    Fin ( Index ) = 0  ! Finalization starts
    PRINT *, "Finalization starts"
  END ASSOCIATE

  !FINALIZATION Finishes
  PRINT *, Fin
  IF ( ANY(Fin .NE. (/0,3,1,1,1,-1/) ) ) STOP 99


  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(DT), INTENT(IN), TARGET :: Arg
  CLASS(DT), POINTER :: Fun
    Fun => Arg
  END FUNCTION


  END

