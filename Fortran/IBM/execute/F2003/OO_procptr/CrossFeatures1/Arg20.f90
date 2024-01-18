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
!*  Argument association - Implicit interface
!*  If the dummy argument is referenced as a subroutine, the actual argumenti
!*  shall be  a subroutine, subroutine procedure pointer, or dummy procedure.
!* (304228)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT TYPE(Base)(P)

    TYPE :: Base
      CHARACTER(3) :: C
    END TYPE

    INTERFACE
      SUBROUTINE IntF(Arg1, Arg2)
      IMPORT
        TYPE(Base), INTENT(IN)  :: Arg2
        TYPE(Base), INTENT(OUT) :: Arg1
      END SUBROUTINE
    END INTERFACE


  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(Base), INTENT(IN)  :: Arg2
  TYPE(Base), INTENT(OUT) :: Arg1
    Arg1 = Arg2
  END SUBROUTINE


  PROGRAM Arg20
  USE M
  IMPLICIT TYPE(Base)(P)
  PROCEDURE(IntF) :: ExtSub

  CALL IntSub( IntFun1(ExtSub), IntFun1(ExtSub))
  CALL IntSub( IntFun1(ExtSub), IntFun2(ExtSub))


  CONTAINS

  FUNCTION IntFun1(Proc)
  PROCEDURE(IntF) :: Proc
  PROCEDURE(IntF), POINTER :: IntFun1
    IntFun1 => Proc
  END FUNCTION

  FUNCTION IntFun2(Proc)
  PROCEDURE() :: Proc
  PROCEDURE(), POINTER :: IntFun2
    IntFun2 => Proc
  END FUNCTION


  SUBROUTINE IntSub(ProcPtr0, ProcPtr1)
  IMPLICIT TYPE(Base)(P)

  PROCEDURE(IntF),       POINTER :: ProcPtr0
  PROCEDURE(),           POINTER :: ProcPtr1
  TYPE(Base)                     :: V

  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtSub) ) STOP 10
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtSub) ) STOP 11

  CALL ProcPtr0(V, Base("321"))
  IF (V%C .NE. "321") STOP 15

  CALL ProcPtr1(V, Base("123"))
  IF (V%C .NE. "123") STOP 13

  END SUBROUTINE

  END

