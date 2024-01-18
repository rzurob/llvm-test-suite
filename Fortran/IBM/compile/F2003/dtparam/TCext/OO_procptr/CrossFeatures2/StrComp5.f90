! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures2/StrComp5.f
! opt variations: -qnok -ql

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
! %POSTCMD: tcomp  StrComp5.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : StrComp5.f
!*
!*  DATE                       : Jun. 24, 2005
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
!*  1. Proc-ptr name conflict with type bound names
!*  2. pure subprogram
!*  (315494/315506)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: DT1(K1)    ! (4)
      INTEGER, KIND :: K1
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr
    END TYPE

    TYPE, EXTENDS(DT1)  :: DT2    ! (4)
    CONTAINS
      PROCEDURE, PASS :: ProcPtr => ModFun2
    END TYPE

    TYPE  :: DT3(K2)    ! (4)
      INTEGER, KIND :: K2
      PROCEDURE(), NoPASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, NoPASS :: ProcPtr => ModFun3
    END TYPE

    CONTAINS

    FUNCTION ModFun1(Arg)
    CLASS(DT1(4)) :: Arg
    TYPE(DT1(4)) :: ModFun1
      ModFun1 = Arg
    END FUNCTION

    FUNCTION ModFun2(Arg)
    CLASS(DT2(4)) :: Arg
    TYPE(DT2(4)) :: ModFun2
      ModFun2 = Arg
    END FUNCTION

  END MODULE


  PURE SUBROUTINE ExtSub(Proc, ProcPtr)
  PROCEDURE()          :: Proc
  PROCEDURE(), POINTER :: ProcPtr
  END SUBROUTINE

  PURE SUBROUTINE ExtSub1(Proc, ProcPtr)
  INTERFACE
    SUBROUTINE IS()
    END SUBROUTINE
  END INTERFACE
  PROCEDURE(IS)          :: Proc
  PROCEDURE(IS), POINTER :: ProcPtr
  END SUBROUTINE

  PROGRAM StrComp5

  END

