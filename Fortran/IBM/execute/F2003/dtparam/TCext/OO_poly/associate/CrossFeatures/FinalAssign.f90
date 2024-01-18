! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=base /tstdev/OO_poly/associate/CrossFeatures/FinalAssign.f
! opt variations: -qnock -qnok -qnol -qreuse=none

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

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
      CONTAINS
      FINAL :: FinalBase
    END TYPE

    TYPE, EXTENDS(Base) :: DT(K2,N2)    ! (4,20,1,3)
      INTEGER, KIND             :: K2
      INTEGER, LEN              :: N2
      INTEGER(K1)               :: Id = 0
      CHARACTER(kind=K2,len=N2) :: C  = " "
      LOGICAL(K1)               :: L  = .FALSE.

      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL

      FINAL :: Final
      FINAL :: FinalArr

    END TYPE

  CONTAINS

    SUBROUTINE FinalBase(Obj)
    TYPE(Base(4,*)) :: Obj
      Index = Index + 1
      Fin(Index) = 1
!     PRINT *, "Base Finalization"
    END SUBROUTINE

    SUBROUTINE Final(Obj)
    TYPE(DT(4,*,1,*)) :: Obj
      Index = Index + 1
      Fin(Index) = 2
!     PRINT *, "DT Finalization"
    END SUBROUTINE

    SUBROUTINE FinalArr(ObjArr)
    TYPE(DT(4,*,1,*)) :: ObjArr(:)
      Index = Index + 1
      Fin(Index) = 3
!     PRINT *, "DT ARR Finalization"
    END SUBROUTINE

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4,*,1,*))(A)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

    ELEMENTAL FUNCTION GetC(Arg)
    IMPLICIT CLASS(DT(4,*,1,*))(A)
    INTENT(IN) :: Arg
    CHARACTER  :: GetC
      GetC = Arg%C
    END FUNCTION

    ELEMENTAL FUNCTION GetL(Arg)
    IMPLICIT CLASS(DT(4,*,1,*))(A)
    INTENT(IN) :: Arg
    LOGICAL    :: GetL
      GetL = Arg%L
    END FUNCTION

  END MODULE

  PROGRAM FinalAssign
  USE M
  IMPLICIT NONE

  TYPE(DT(4,20,1,3)), TARGET :: V =  DT(4,20,1,3)(ID=-1, C="!", L=.TRUE.)

  Fin = -1
  Index = 0

  ASSOCIATE ( As => V )

  ! no finalization happen for a return of a pointer
    IF ( ANY(Fin .NE. -1 ) ) STOP 19

    ASSOCIATE ( As => As )

      As = DT(4,20,1,3)(ID=-2, C="2", L=.TRUE.)
      !FINALIZATION Finished  for the left side
      IF ( ANY(Fin .NE. (/2,1,2,1,-1,-1/) ) ) STOP 18
      Fin = -1
      Index = 1

      IF ( As%ID       .NE. -2 ) STOP 20
      IF ( As%GetID()  .NE. -2 ) STOP 21

      IF ( As%C       .NE. "2" ) STOP 30
      IF ( As%GetC()  .NE. "2" ) STOP 31

      IF ( As%L       .NEQV. .TRUE. ) STOP 40
      IF ( As%GetL()  .NEQV. .TRUE. ) STOP 41

    END ASSOCIATE

    ! No FINALIZATION
    IF ( ANY(Fin .NE. -1 ) ) STOP 17

  END ASSOCIATE

  ! No FINALIZATION
  IF ( ANY(Fin .NE. -1 ) ) STOP 16

  END

