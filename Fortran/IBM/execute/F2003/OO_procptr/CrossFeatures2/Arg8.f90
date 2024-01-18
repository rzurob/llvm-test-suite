! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 27, 2005
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
!* Argument association - intent
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    USE ISO_C_BINDING

    TYPE, BIND(C) :: DT
      CHARACTER(C_CHAR) :: C
    END TYPE

    CONTAINS

    PURE FUNCTION ModFun(Arg)
    TYPE(DT), INTENT(IN) :: Arg
    TYPE(DT)             ::  ModFun
      ModFun=Arg
    END FUNCTION

    PURE FUNCTION IFun(Arg)
    TYPE(DT), INTENT(IN) :: Arg
    TYPE(DT)             :: IFun
      IFun=Arg
    END FUNCTION

  END MODULE

  PROGRAM Intent0
  USE M
  IMPLICIT NONE
  PROCEDURE(IFun), POINTER :: ProcPtr

  ProcPtr => Modfun
  CALL ModSub1(ProcPtr, Modfun)
  CALL ModSub2(ProcPtr, ModFun)
  CALL ModSub3(ProcPtr, ModFun)


  CONTAINS

    SUBROUTINE ModSub1(ProcPtr, Proc)
    PROCEDURE(IFun), POINTER, INTENT(IN) :: ProcPtr
    PROCEDURE(IFun)                      :: Proc
    TYPE(DT)                             :: V
      IF (.NOT. ASSOCIATED(ProcPtr, ModFun)) STOP 11
      V = ProcPtr(DT("1"))
      IF ( V%C .NE. "1" ) STOP 12
    END SUBROUTINE

    SUBROUTINE ModSub2(ProcPtr, Proc)
    IMPLICIT TYPE(DT) (P)
    PROCEDURE(TYPE(DT)), POINTER, INTENT(INOUT) :: ProcPtr
    PROCEDURE()                                 :: Proc
    TYPE(DT)                                    :: V
      V=Proc(DT("2"))
      ProcPtr => Proc
      IF (.NOT. ASSOCIATED(ProcPtr, ModFun)) STOP 21
      V = ProcPtr(DT("1"))
      IF ( V%C .NE. "1" ) STOP 22
    END SUBROUTINE

    SUBROUTINE ModSub3(ProcPtr, Proc)
    IMPLICIT TYPE(DT) (P)
    PROCEDURE(), POINTER, INTENT(OUT) :: ProcPtr
    PROCEDURE(TYPE(DT))               :: Proc
    TYPE(DT)                          :: V
      ProcPtr => Proc
      IF (.NOT. ASSOCIATED(ProcPtr, ModFun)) STOP 31
      V = ProcPtr(DT("1"))
      IF ( V%C .NE. "1" ) STOP 32
    END SUBROUTINE

  END


