! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  FinalIntentOut.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FinalIntentOut
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
!*   The intent(out) attribute
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

  PROGRAM FinalIntentOut
  USE M
  IMPLICIT NONE

  TYPE(DT), TARGET :: V =  DT(ID=-1, C="!", L=.TRUE.)

  Fin = -1

  ASSOCIATE ( As => Fun(V) )

    !FINALIZATION Finishes after evaluation of the selector
    IF ( ANY(Fin .NE. (/2,1,2,1,-1,-1/) ) ) STOP 79

    Fin = -1
    ASSOCIATE ( As => As )

      IF ( As%ID       .NE. -2 ) STOP 20
      IF ( As%GetID()  .NE. -2 ) STOP 21

      IF ( As%C       .NE. "2" ) STOP 30
      IF ( As%GetC()  .NE. "2" ) STOP 31

      IF ( As%L       .NEQV. .TRUE. ) STOP 40
      IF ( As%GetL()  .NEQV. .TRUE. ) STOP 41

    END ASSOCIATE

    IF ( ANY(Fin .NE. -1 ) ) STOP 89 !  no finalization happen

    INDEX = 1
    Fin ( Index ) = 0  ! Finalization is about to start
    PRINT *, "Finalization starts"
  END ASSOCIATE

  !FINALIZATION Finishes
  PRINT *, "Finalization finished"
  PRINT *, Fin
  IF ( ANY(Fin .NE. (/0,-1,-1,-1,-1,-1/) ) ) STOP 99
  ! no finalization happen for a return of a pointer

  Fin = -1
  Index = 0
  ASSOCIATE ( As => V )

    ASSOCIATE ( As => Fun(As) )

      !FINALIZATION Finishes after evaluation of the selector
      IF ( ANY(Fin .NE. (/2,1,2,1,-1,-1/) ) ) STOP 119

      IF ( As%ID       .NE. -2 ) STOP 50
      IF ( As%GetID()  .NE. -2 ) STOP 51

      IF ( As%C       .NE. "2" ) STOP 60
      IF ( As%GetC()  .NE. "2" ) STOP 61

      IF ( As%L       .NEQV. .TRUE. ) STOP 70
      IF ( As%GetL()  .NEQV. .TRUE. ) STOP 71

    END ASSOCIATE

    !FINALIZATION Finishes
    PRINT *, "Finalization finished"
    IF ( ANY(Fin .NE. (/2,1,2,1,-1,-1/) ) ) STOP 89
    ! no finalization happen for a return of a pointer

    Fin = -1
    INDEX = 1
    Fin ( Index ) = 0

  END ASSOCIATE

  IF ( ANY(Fin .NE. (/0,-1,-1,-1,-1,-1/) ) ) STOP 109
  ! no finalization happen for a return of a pointer

  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(DT), INTENT(OUT) :: Arg
  CLASS(DT), POINTER :: Fun
    print*, "in fun"
    ALLOCATE(Fun, SOURCE= DT(ID=-2, C="2", L=.TRUE.))
    print*, "out fun"
  END FUNCTION


  END

