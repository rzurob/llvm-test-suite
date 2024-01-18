! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/Misc1.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc1.f
!*
!*  DATE                       : May. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Procedure pointer - dtio
!*  (314836)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr
      CONTAINS
      PROCEDURE, PASS :: Proc => ModFun

    END TYPE

    TYPE(DT(20,4)) :: Mark(100)=DT(20,4)(0, NULL())
    INTEGER  :: Index=0

    INTERFACE  WRITE (FORMATTED)
      MODULE PROCEDURE my_write_routine_formatted
    END INTERFACE

  CONTAINS

    SUBROUTINE my_write_routine_formatted               &
             & (dtv, unit, iotype, v_list,  iostat, iomsg)
    CLASS(DT(*,4)) ,   INTENT(IN)    :: dtv
    INTEGER,           INTENT(IN)    :: unit
    CHARACTER (LEN=*), INTENT(IN)    :: iotype
    INTEGER,           INTENT(IN)    :: v_list(:)
    INTEGER,           INTENT(OUT)   :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg

    Index = Index + 1
    Mark(Index)%ID = dtv%ID
    Mark(Index)%PRocPtr => dtv%ProcPtr

    END SUBROUTINE

    SUBROUTINE ModSub()
    END SUBROUTINE

    FUNCTION ModFun(Arg)
    CLASS(DT(*,4)), TARGET  :: Arg
    CLASS(DT(:,4)), POINTER :: ModFun
      ModFun => Arg
    END FUNCTION

  END MODULE

  PROGRAM Misc1
  USE M
  IMPLICIT TYPE(DT(20,4))(P)

  PROCEDURE(ModFun),        POINTER :: ProcPtr1
  PROCEDURE(ProcPtr1),      POINTER :: ProcPtr2
  TYPE(DT(20,4))                          :: Const=DT(20,4)(-1, NULL())


  TYPE(DT(20,4))         :: V(100)
  TYPE(DT(20,4)), TARGET :: Tar

  Const%ProcPtr => ModFun
  Tar = Const
  V = Tar

  CALL Clear()
  PRINT *, TAR
  IF ( Index .NE. 1 )                             STOP 10
  IF ( Mark(1)%ID .NE. -1 )                       STOP 11
  IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) STOP 12

  CALL Clear()
  ProcPtr1 => ModFun
  PRINT *, ProcPtr1(Tar)
  IF ( Index .NE. 1 )                             STOP 20
  IF ( Mark(1)%ID .NE. -1 )                       STOP 21
  IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) STOP 22

  CALL Clear()
  ProcPtr2 => ProcPtr1
  PRINT *, ProcPtr2(Tar)
  IF ( Index .NE. 1 )                             STOP 30
  IF ( Mark(1)%ID .NE. -1 )                       STOP 31
  IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) STOP 32

  DO I=1, 100

    CALL Clear()
    PRINT *, V(I)%ProcPtr()
    IF ( Index .NE. 1 )                             STOP 40
    IF ( Mark(1)%ID .NE. -1 )                       STOP 41
    IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) STOP 42

    CALL Clear()
    PRINT *, V(I)%Proc()
    IF ( Index .NE. 1 )                             STOP 50
    IF ( Mark(1)%ID .NE. -1 )                       STOP 51
    IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) STOP 52

  END DO

  CALL Clear()
  PRINT *, V
  IF ( Index .NE. 100 )                           STOP 60

  IF ( ANY(Mark%ID .NE. -1 ) )                    STOP 61

  CONTAINS

  SUBROUTINE Clear()
    MARK = DT(20,4)(0, NULL())
    INDEX = 0
  END SUBROUTINE
  END

