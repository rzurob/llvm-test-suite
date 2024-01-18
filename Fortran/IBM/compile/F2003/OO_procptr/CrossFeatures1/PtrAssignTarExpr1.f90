! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp PtrAssignTarExpr1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PrtAssignTarExpr1.f
!*
!*  DATE                       : Mar. 12, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  C726 (R742) An expr shall be a reference to a function whose result
!*  is a procedure pointer.
!*
!*  (306632)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

    FUNCTION IFun()
    INTEGER, POINTER :: IFun
      !ALLOCATE(IFun, SOURCE=11)
      ALLOCATE(IFun)
      IFun = 11
    END FUNCTION

    FUNCTION RFun()
    REAL, POINTER :: RFun
      !ALLOCATE(RFun, SOURCE=1.0)
      ALLOCATE(RFun)
      RFun = 1.0
    END FUNCTION

    FUNCTION CFun()
    CHARACTER(1), POINTER :: CFun
      !ALLOCATE(CFun, SOURCE="X")
      ALLOCATE(CFun)
      CFun = "X"
    END FUNCTION

    FUNCTION DFun()
    DOUBLE PRECISION, POINTER :: DFun
      !ALLOCATE(DFun, SOURCE=1D0)
      ALLOCATE(DFun)
      DFun = 1D0
    END FUNCTION

    FUNCTION LFun()
    LOGICAL, POINTER :: LFun
      !ALLOCATE(LFun, SOURCE=.TRUE.)
      ALLOCATE(LFun)
      LFun = .TRUE.
    END FUNCTION

    FUNCTION BFun()
    BYTE, POINTER :: BFun
      !ALLOCATE(BFun, SOURCE=1_1)
      ALLOCATE(BFun)
      BFun = 1_1
    END FUNCTION

  END MODULE


  PROGRAM PrtAssignTarExpr1
  USE M
  IMPLICIT NONE

  PROCEDURE(INTEGER),          POINTER :: IPtr
  PROCEDURE(REAL),             POINTER :: RPtr
  PROCEDURE(CHARACTER),        POINTER :: CPtr
  PROCEDURE(DOUBLE PRECISION), POINTER :: DPtr
  PROCEDURE(LOGICAL),          POINTER :: LPtr
  PROCEDURE(Byte),             POINTER :: BPtr

    IPtr  => IFun()

    RPtr  => RFun()

    CPtr  => CFun()

    DPtr  => DFun()

    LPtr  => LFun()

    BPtr  => BFun()

  END

