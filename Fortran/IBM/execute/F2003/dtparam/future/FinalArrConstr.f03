! GB DTP extension using:
! ftcx_dtp -qk -qnol -qdeferredlp -qreuse=base /tstdev/OO_poly/associate/CrossFeatures/FinalArrConstr.f
! opt variations: -qck -qnok -ql -qnodeferredlp -qreuse=none

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
!*   The finalization-Array  constructor (Interp-301240)
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTEGER :: INDEX=0
    INTEGER :: Fin(6)=0

    TYPE :: Base(K1)    ! (4)
        INTEGER, KIND :: K1
      CONTAINS
      FINAL :: FinalBase
    END TYPE

    TYPE, EXTENDS(Base) :: DT(N1)    ! (4,3)
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id = 0
      CHARACTER(N1) :: C  = " "
      LOGICAL(K1)   :: L  = .FALSE.

      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL

      FINAL :: Final
      FINAL :: FinalArr

    END TYPE

  CONTAINS

    SUBROUTINE FinalBase(Obj)
    TYPE(Base(4)) :: Obj
      Index = Index + 1
      Fin(Index) = 1
      PRINT *, "Base Finalization"
    END SUBROUTINE

    SUBROUTINE Final(Obj)
    TYPE(DT(4,*)) :: Obj
      Index = Index + 1
      Fin(Index) = 2
      PRINT *, "DT Finalization"
    END SUBROUTINE

    SUBROUTINE FinalArr(ObjArr)
    TYPE(DT(4,*)) :: ObjArr(:)
      Index = Index + 1
      Fin(Index) = 3
      PRINT *, "DT ARR Finalization"
    END SUBROUTINE

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4,*))(A)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

    ELEMENTAL FUNCTION GetC(Arg)
    IMPLICIT CLASS(DT(4,*))(A)
    INTENT(IN) :: Arg
    CHARACTER  :: GetC
      GetC = Arg%C
    END FUNCTION

    ELEMENTAL FUNCTION GetL(Arg)
    IMPLICIT CLASS(DT(4,*))(A)
    INTENT(IN) :: Arg
    LOGICAL    :: GetL
      GetL = Arg%L
    END FUNCTION

  END MODULE

  PROGRAM FinalArrConstr

  USE M
  IMPLICIT TYPE(DT(4,3))(F)

  TYPE(DT(4,3)), TARGET :: V =  DT(4,3)(ID=-1, C="!", L=.TRUE.)

  Fin = -1

  ASSOCIATE ( As => (/Fun(V),Fun(V),Fun(V)/) )

    IF ( ANY(SHAPE(As) .NE. (/3/)) ) ERROR STOP 30

    IF ( ANY(As%ID       .NE. -1 )) ERROR STOP 20
    IF ( ANY(As%GetID()  .NE. -1 )) ERROR STOP 21

    IF ( ANY(As%C       .NE. "!" )) ERROR STOP 30
    IF ( ANY(As%GetC()  .NE. "!" )) ERROR STOP 31

    IF ( ANY(As%L       .NEQV. .TRUE. )) ERROR STOP 60
    IF ( ANY(As%GetL()  .NEQV. .TRUE. )) ERROR STOP 61

    INDEX = 1
    Fin ( Index ) = 0  ! Finalization starts
    PRINT *, "Finalization starts"
  END ASSOCIATE

  !FINALIZATION Finishes
  PRINT *, Fin
  IF ( ANY(Fin .NE. (/0,3,1,1,1,-1/) ) ) ERROR STOP 99


  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(DT(4,*)), INTENT(IN), TARGET :: Arg
  CLASS(DT(4,:)), POINTER :: Fun
    Fun => Arg
  END FUNCTION


  END
