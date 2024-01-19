! *********************************************************************
!*  ===================================================================
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


  PROGRAM Arg15
  USE M
  IMPLICIT TYPE(Base)(P)
  PROCEDURE(IntF) :: ExtFun
  PROCEDURE(IntF),   POINTER :: ProcPtr0
  PROCEDURE(ExtFun), POINTER :: ProcPtr1
  PROCEDURE(),       POINTER :: ProcPtr2

  ProcPtr0 => ExtFun
  ProcPtr1 => ExtFun
  ProcPtr2 => ExtFun

  CALL IntSub( ProcPtr0, ProcPtr1, ProcPtr2)


  CONTAINS

  SUBROUTINE IntSub(ProcPtr0, ProcPtr1, ProcPtr2)
  IMPLICIT TYPE(Base)(P)

  PROCEDURE(IntF),       POINTER :: ProcPtr0
  PROCEDURE(TYPE(Base)), POINTER :: ProcPtr1
  PROCEDURE(),           POINTER :: ProcPtr2
  TYPE(Base)                     :: V

  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtFun) ) ERROR STOP 10
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtFun) ) ERROR STOP 11
  IF ( .NOT. ASSOCIATED(ProcPtr2, ExtFun) ) ERROR STOP 12

  V = ProcPtr0(Base("321"))
  IF (V%C .NE. "321") ERROR STOP 15

  V = ProcPtr1(Base("123"))
  IF (V%C .NE. "123") ERROR STOP 13

  V = ProcPtr2(Base("abc"))
  IF (V%C .NE. "abc") ERROR STOP 14

  END SUBROUTINE

  END

