! *********************************************************************
!*  ===================================================================
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


  PROGRAM Arg16
  USE M
  IMPLICIT TYPE(Base)(P)
  PROCEDURE(IntF) :: ExtFun

  CALL IntSub(ExtFun, ExtFun, ExtFun)


  CONTAINS

  SUBROUTINE IntSub(Proc0, Proc1, Proc2)
  IMPLICIT TYPE(Base)(P)

  PROCEDURE(IntF)       :: Proc0
  PROCEDURE(TYPE(Base)) :: Proc1
  PROCEDURE()           :: Proc2
  TYPE(Base)            :: V

  V = Proc0(Base("321"))
  IF (V%C .NE. "321") ERROR STOP 15

  V = Proc1(Base("123"))
  IF (V%C .NE. "123") ERROR STOP 13

  V = Proc2(Base("abc"))
  IF (V%C .NE. "abc") ERROR STOP 14

  END SUBROUTINE

  END

