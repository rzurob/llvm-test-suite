! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Arg18.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg18.f
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
!*  Argument association - Implicit interface
!*  Func return as actual arg
!* (306474)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT TYPE(Base)(P)

    TYPE :: Base
      CHARACTER(3) :: C
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base), INTENT(IN) :: Arg
        TYPE(Base):: IntF
      END FUNCTION
    END INTERFACE


  END MODULE

  PURE FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base), INTENT(IN) :: Arg
  TYPE(Base) :: ExtFun
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg19
  USE M
  IMPLICIT TYPE(Base)(P)
  PROCEDURE(IntF) :: ExtFun

  CALL IntSub( IntFun1(ExtFun), IntFun1(ExtFun), IntFun1(ExtFun))
  CALL IntSub( IntFun1(ExtFun), IntFun2(ExtFun), IntFun2(ExtFun))
  CALL IntSub( IntFun1(ExtFun), IntFun3(ExtFun), IntFun3(ExtFun))


  CONTAINS

  FUNCTION IntFun1(Proc)
  PROCEDURE(IntF) :: Proc
  PROCEDURE(IntF), POINTER :: IntFun1
    IntFun1 => Proc
  END FUNCTION

  FUNCTION IntFun2(Proc)
  PROCEDURE(TYPE(Base)) :: Proc
  PROCEDURE(TYPE(Base)), POINTER :: IntFun2
    IntFun2 => Proc
  END FUNCTION

  FUNCTION IntFun3(Proc)
  IMPLICIT TYPE(Base)(P,I)
  PROCEDURE() :: Proc
  PROCEDURE(), POINTER :: IntFun3
    IntFun3 => Proc
  END FUNCTION

  SUBROUTINE IntSub(ProcPtr0, ProcPtr1, ProcPtr2)
  IMPLICIT TYPE(Base)(P)

  PROCEDURE(IntF),       POINTER :: ProcPtr0
  PROCEDURE(TYPE(Base)), POINTER :: ProcPtr1
  PROCEDURE(),           POINTER :: ProcPtr2
  TYPE(Base)                     :: V

  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtFun) ) STOP 10
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtFun) ) STOP 11
  IF ( .NOT. ASSOCIATED(ProcPtr2, ExtFun) ) STOP 12

  V = ProcPtr0(Base("321"))
  IF (V%C .NE. "321") STOP 15

  V = ProcPtr1(Base("123"))
  IF (V%C .NE. "123") STOP 13

  V = ProcPtr2(Base("abc"))
  IF (V%C .NE. "abc") STOP 14

  END SUBROUTINE

  END

