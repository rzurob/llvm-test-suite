! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 20, 2005
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
!*  Dummy procedure - dummy or intrinsic procedure
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE :: Base
      CHARACTER(3) :: C
      TYPE(Base), POINTER :: BPtr
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base) :: Arg, IntF
      END FUNCTION
    END INTERFACE

  END MODULE

  MODULE M
  USE M0

    CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(Base) :: Arg, ModFun
      ModFun = Arg
    END FUNCTION

    SUBROUTINE ModSub(Fun1, Fun2)
    IMPLICIT TYPE(Base)(F)
    PROCEDURE(IntF) :: Fun1
    PROCEDURE(  ) :: Fun2
    TYPE(Base), TARGET :: Tar=Base("TAR", NULL())
    TYPE(Base) :: V, U
    PROCEDURE(IntF), POINTER :: ProcPtr1
    PROCEDURE(IntF), POINTER :: ProcPtr2


    ProcPtr1 => Fun1
    IF ( .NOT. ASSOCIATED(ProcPtr1, Fun2)) ERROR STOP 71

    ProcPtr2 => ProcPtr1
    IF ( .NOT. ASSOCIATED(ProcPtr2, Fun1)) ERROR STOP 72

    V = Fun1(Base("abc", Tar))
    IF ( V%C .NE. "abc") ERROR STOP 11
    IF ( .NOT. ASSOCIATED(V%BPtr, Tar)) ERROR STOP 13

    U = ProcPtr1(Base("abc",Tar))
    IF ( V%C .NE. "abc") ERROR STOP 21
    IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) ERROR STOP 23

    U = Fun2(Base("123",Tar))
    IF ( U%C .NE. "123") ERROR STOP 31
    IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) ERROR STOP 33

    U = ProcPtr2(Base("321",Tar))
    IF ( U%C .NE. "321") ERROR STOP 31
    IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) ERROR STOP 33

    END SUBROUTINE


  END MODULE

  PROGRAM Arg3
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION RToR(Arg)
      REAL :: RToT, Arg
    END FUNCTION
  END INTERFACE

  INTRINSIC :: SIN, SQRT, ALOG10, ACOS

  CALL IntSub(ModFun, ModFun)

  IF ( ABS(IntFun(SIN,1.0)-0.84147098) .GT. 1.0E-6) ERROR STOP 55
  IF ( ABS(IntFun(SQRT,4.0)-2.0) .GT. 1.0E-6)       ERROR STOP 56
  IF ( ABS(IntFun(ALOG10,10.0)-1.0) .GT. 1.0E-6)    ERROR STOP 57
  IF ( ABS(IntFun(ACOS, 0.54030231)-1.0) .GT. 1.0E-6)    ERROR STOP 58

  CONTAINS

  SUBROUTINE IntSub(Fun1, Fun2)
  PROCEDURE(IntF) :: Fun1, Fun2
    CALL ModSub(Fun1, Fun2)
  END SUBROUTINE

  FUNCTION IntFun(Fun, Arg)
  PROCEDURE(RToR) :: Fun
  REAL :: IntFun, Arg
    IntFun =Fun(Arg)
  END FUNCTION

  END
