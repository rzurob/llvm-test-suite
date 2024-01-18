! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures1/Arg11.f
! opt variations: -qck -qnok

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Arg11.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg11.f
!*
!*  DATE                       : May. 23, 2005
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
!*  Explicit dummy procedure - Characteristics
!*  Pure/! Ext. Elemental is illegal
!* (Memory fault)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE

    INTERFACE
      PURE FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(4,*)), INTENT(IN) :: Arg
        TYPE(Base(4,3)):: IntF(2:4)
      END FUNCTION
    END INTERFACE

  END MODULE

  PURE FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(4,*)), INTENT(IN) :: Arg
  TYPE(Base(4,3)) :: ExtFun(3)
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg11
  USE M
  IMPLICIT NONE

  PROCEDURE(IntF) :: ExtFun

  PROCEDURE(IntF), POINTER :: ProcPtr
! INTRINSIC :: ABS  !for 11.1


  CALL IntSub(ExtFun)

  ProcPtr => ExtFun
  CALL IntSub(ProcPtr)

  CONTAINS

    SUBROUTINE IntSub(Arg)
    IMPLICIT TYPE(Base(4,3))(A)

    INTERFACE
      FUNCTION IntF3(Arg1)
      IMPORT
        TYPE(Base(4,*)) :: Arg1
        TYPE(Base(4,3)):: IntF3(2:4)
      END FUNCTION
    END INTERFACE

    PROCEDURE(IntF3) :: Arg
    TYPE(Base(4,3)) :: V(3)

    V = Arg(Base(4,3)("123"))
    IF (ANY(V%C .NE. "123"))             STOP 11

    END SUBROUTINE


  END

