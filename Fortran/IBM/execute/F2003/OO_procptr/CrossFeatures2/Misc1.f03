! *********************************************************************
!*  ===================================================================
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

    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr
      CONTAINS
      PROCEDURE, PASS :: Proc => ModFun

    END TYPE

    TYPE(DT) :: Mark(100)=DT(0, NULL())
    INTEGER  :: Index=0

    INTERFACE  WRITE (FORMATTED)
      MODULE PROCEDURE my_write_routine_formatted
    END INTERFACE

  CONTAINS

    SUBROUTINE my_write_routine_formatted               &
             & (dtv, unit, iotype, v_list,  iostat, iomsg)
    CLASS(DT) ,        INTENT(IN)    :: dtv
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
    CLASS(DT), TARGET  :: Arg
    CLASS(DT), POINTER :: ModFun
      ModFun => Arg
    END FUNCTION

  END MODULE

  PROGRAM Misc1
  USE M
  IMPLICIT TYPE(DT)(P)

  PROCEDURE(ModFun),        POINTER :: ProcPtr1
  PROCEDURE(ProcPtr1),      POINTER :: ProcPtr2
  TYPE(DT)                          :: Const=DT(-1, NULL())


  TYPE(DT)         :: V(100)
  TYPE(DT), TARGET :: Tar

  Const%ProcPtr => ModFun
  Tar = Const
  V = Tar

  CALL Clear()
  PRINT *, TAR
  IF ( Index .NE. 1 )                             ERROR STOP 10
  IF ( Mark(1)%ID .NE. -1 )                       ERROR STOP 11
  IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) ERROR STOP 12

  CALL Clear()
  ProcPtr1 => ModFun
  PRINT *, ProcPtr1(Tar)
  IF ( Index .NE. 1 )                             ERROR STOP 20
  IF ( Mark(1)%ID .NE. -1 )                       ERROR STOP 21
  IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) ERROR STOP 22

  CALL Clear()
  ProcPtr2 => ProcPtr1
  PRINT *, ProcPtr2(Tar)
  IF ( Index .NE. 1 )                             ERROR STOP 30
  IF ( Mark(1)%ID .NE. -1 )                       ERROR STOP 31
  IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) ERROR STOP 32

  DO I=1, 100

    CALL Clear()
    PRINT *, V(I)%ProcPtr()
    IF ( Index .NE. 1 )                             ERROR STOP 40
    IF ( Mark(1)%ID .NE. -1 )                       ERROR STOP 41
    IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) ERROR STOP 42

    CALL Clear()
    PRINT *, V(I)%Proc()
    IF ( Index .NE. 1 )                             ERROR STOP 50
    IF ( Mark(1)%ID .NE. -1 )                       ERROR STOP 51
    IF ( .NOT. ASSOCIATED(MARK(1)%ProcPtr, ModFun)) ERROR STOP 52

  END DO

  CALL Clear()
  PRINT *, V
  IF ( Index .NE. 100 )                           ERROR STOP 60

  IF ( ANY(Mark%ID .NE. -1 ) )                    ERROR STOP 61

  CONTAINS

  SUBROUTINE Clear()
    MARK = DT(0, NULL())
    INDEX = 0
  END SUBROUTINE
  END
