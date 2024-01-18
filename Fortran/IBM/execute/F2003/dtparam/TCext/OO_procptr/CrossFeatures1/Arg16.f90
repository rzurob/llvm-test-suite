! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/Arg16.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Arg16.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg16.f
!*
!*  DATE                       : May. 25, 2005
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
!*  Argument association -
!*  Implicit interface
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT TYPE(Base(1,3))(P)

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(1,*)), INTENT(IN) :: Arg
        TYPE(Base(1,3)):: IntF
      END FUNCTION
    END INTERFACE


  END MODULE

  PURE FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(1,*)), INTENT(IN) :: Arg
  TYPE(Base(1,3)) :: ExtFun
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg16
  USE M
  IMPLICIT TYPE(Base(1,3))(P)
  PROCEDURE(IntF) :: ExtFun

  CALL IntSub(ExtFun, ExtFun, ExtFun)


  CONTAINS

  SUBROUTINE IntSub(Proc0, Proc1, Proc2)
  IMPLICIT TYPE(Base(1,3))(P)

  PROCEDURE(IntF)   :: Proc0
  PROCEDURE(ExtFun) :: Proc1
  PROCEDURE(IntF)   :: Proc2
  TYPE(Base(1,3))   :: V

  V = Proc0(Base(1,3)("321"))
  IF (V%C .NE. "321") STOP 15

  V = Proc1(Base(1,3)("123"))
  IF (V%C .NE. "123") STOP 13

  V = Proc2(Base(1,3)("abc"))
  IF (V%C .NE. "abc") STOP 14

  END SUBROUTINE

  END

