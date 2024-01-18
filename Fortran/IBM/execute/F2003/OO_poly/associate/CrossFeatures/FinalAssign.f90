! *********************************************************************
!*  ===================================================================
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
!*   The intrinsic assignment
!*    (Failed 18-301309)
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

  PROGRAM FinalAssign
  USE M
  IMPLICIT NONE

  TYPE(DT), TARGET :: V =  DT(ID=-1, C="!", L=.TRUE.)

  Fin = -1
  Index = 0

  ASSOCIATE ( As => V )

  ! no finalization happen for a return of a pointer
    IF ( ANY(Fin .NE. -1 ) ) ERROR STOP 19

    ASSOCIATE ( As => As )

      As = DT(ID=-2, C="2", L=.TRUE.)
      !FINALIZATION Finished  for the left side
      IF ( ANY(Fin .NE. (/2,1,2,1,-1,-1/) ) ) ERROR STOP 18
      Fin = -1
      Index = 1

      IF ( As%ID       .NE. -2 ) ERROR STOP 20
      IF ( As%GetID()  .NE. -2 ) ERROR STOP 21

      IF ( As%C       .NE. "2" ) ERROR STOP 30
      IF ( As%GetC()  .NE. "2" ) ERROR STOP 31

      IF ( As%L       .NEQV. .TRUE. ) ERROR STOP 40
      IF ( As%GetL()  .NEQV. .TRUE. ) ERROR STOP 41

    END ASSOCIATE

    ! No FINALIZATION
    IF ( ANY(Fin .NE. -1 ) ) ERROR STOP 17

  END ASSOCIATE

  ! No FINALIZATION
  IF ( ANY(Fin .NE. -1 ) ) ERROR STOP 16

  END

