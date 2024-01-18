! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/Arg15.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Arg15.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg15.f
!*
!*  DATE                       : May. 24, 2005
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
!* (304184)
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


  PROGRAM Arg15
  USE M
  IMPLICIT TYPE(Base(1,3))(P)
  PROCEDURE(IntF) :: ExtFun
  PROCEDURE(IntF),   POINTER :: ProcPtr0
  PROCEDURE(ExtFun), POINTER :: ProcPtr1
  PROCEDURE(IntF),   POINTER :: ProcPtr2

  ProcPtr0 => ExtFun
  ProcPtr1 => ExtFun
  ProcPtr2 => ExtFun

  CALL IntSub( ProcPtr0, ProcPtr1, ProcPtr2)


  CONTAINS

  SUBROUTINE IntSub(ProcPtr0, ProcPtr1, ProcPtr2)
  IMPLICIT TYPE(Base(1,3))(P)

  PROCEDURE(IntF), POINTER :: ProcPtr0
  PROCEDURE(IntF), POINTER :: ProcPtr1
  PROCEDURE(IntF), POINTER :: ProcPtr2
  TYPE(Base(1,3))          :: V

  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtFun) ) STOP 10
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtFun) ) STOP 11
  IF ( .NOT. ASSOCIATED(ProcPtr2, ExtFun) ) STOP 12

  V = ProcPtr0(Base(1,3)("321"))
  IF (V%C .NE. "321") STOP 15

  V = ProcPtr1(Base(1,3)("123"))
  IF (V%C .NE. "123") STOP 13

  V = ProcPtr2(Base(1,3)("abc"))
  IF (V%C .NE. "abc") STOP 14

  END SUBROUTINE

  END

