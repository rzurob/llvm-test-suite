! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qreuse=base /tstdev/OO_poly/associate/CrossFeatures/FinalFunc.f
! opt variations: -qnock -qnok -ql -qreuse=none

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
!*   The finalization
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

    TYPE, EXTENDS(Base) :: DT(K2,N1)    ! (4,1,3)
      INTEGER, KIND             :: K2
      INTEGER, LEN              :: N1
      INTEGER(K1)               :: Id = 0
      CHARACTER(kind=K2,len=N1) :: C  = " "
      LOGICAL(K1)               :: L  = .FALSE.

      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL

      FINAL :: Final

    END TYPE

  CONTAINS

    SUBROUTINE FinalBase(Obj)
    TYPE(Base(4)) :: Obj
      Index = Index + 1
      Fin(Index) = 1
!     PRINT *, "Base Finalization"
    END SUBROUTINE

    SUBROUTINE Final(Obj)
    TYPE(DT(4,1,*)) :: Obj
      Index = Index + 1
      Fin(Index) = 2
!     PRINT *, "DT Finalization"
    END SUBROUTINE

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4,1,*))(A)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

    ELEMENTAL FUNCTION GetC(Arg)
    IMPLICIT CLASS(DT(4,1,*))(A)
    INTENT(IN) :: Arg
    CHARACTER  :: GetC
      GetC = Arg%C
    END FUNCTION

    ELEMENTAL FUNCTION GetL(Arg)
    IMPLICIT CLASS(DT(4,1,*))(A)
    INTENT(IN) :: Arg
    LOGICAL    :: GetL
      GetL = Arg%L
    END FUNCTION

  END MODULE

  PROGRAM FinalFunc

  USE M
  IMPLICIT TYPE(DT(4,1,3))(F)

  TYPE(DT(4,1,3)) :: V =  DT(4,1,3)(ID=-1, C="!", L=.TRUE.)

  Fin = -1

  ASSOCIATE ( As => Fun(Fun(V)) )
  !ASSOCIATE ( As => Fun(V) )

    IF ( As%ID       .NE. -1 ) STOP 20
    IF ( As%GetID()  .NE. -1 ) STOP 21

    IF ( As%C       .NE. "!" ) STOP 30
    IF ( As%GetC()  .NE. "!" ) STOP 31

    IF ( As%L       .NEQV. .TRUE. ) STOP 60
    IF ( As%GetL()  .NEQV. .TRUE. ) STOP 61

    INDEX = 1
    Fin ( Index ) = 0  ! Finalization starts
!   PRINT *, "Finalization starts"
  END ASSOCIATE

  !FINALIZATION
! PRINT *, Fin
  IF ( ANY(Fin .NE. (/0,2,1,2,1,-1/) ) ) STOP 99


  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(DT(4,1,*)), INTENT(IN) :: Arg
  TYPE(DT(4,1,3)) :: Fun
    Fun = Arg
  END FUNCTION


  END

