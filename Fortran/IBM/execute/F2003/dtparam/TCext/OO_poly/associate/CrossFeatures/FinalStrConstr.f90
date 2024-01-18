! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/associate/CrossFeatures/FinalStrConstr.f
! opt variations: -qck -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  FinalStrConstr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FinalStrConstr
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
!*   The finalization-Structure constructor
!*    (ICE-301244)
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
      INTEGER, LEN   :: N1
      INTEGER(K1)    :: Id = 0
      CHARACTER(N1)  :: C  = " "
      LOGICAL(K1)    :: L  = .FALSE.
      TYPE(Base(K1)) :: BaseComp=Base(K1)()
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
!     PRINT *, "Base Finalization"
    END SUBROUTINE

    SUBROUTINE Final(Obj)
    TYPE(DT(4,*)) :: Obj
      Index = Index + 1
      Fin(Index) = 2
!     PRINT *, "DT Finalization"
    END SUBROUTINE

    SUBROUTINE FinalArr(ObjArr)
    TYPE(DT(4,*)) :: ObjArr(:)
      Index = Index + 1
      Fin(Index) = 3
!     PRINT *, "DT ARR Finalization"
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

  PROGRAM FinalStrConstr

  USE M
  IMPLICIT NONE

  TYPE(DT(4,3)), TARGET :: V =  DT(4,3)(ID=-1, C="!", L=.TRUE.)

  Fin = -1

  ASSOCIATE ( As =>  DT(4,3)(ID=-1, C="!", L=.TRUE., BaseComp=Base(4)()) )

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

  !FINALIZATION Finishes
! PRINT *, Fin
  IF ( ANY(Fin .NE. (/0,2,1,1,1,-1/) ) ) STOP 99

  END

