! GB DTP extension using:
! ftcx_dtp -qck -qnodefaultpv -qdeferredlp /tstdev/OO_procptr/CrossFeatures1/Arg3.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnock -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Arg3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg3.f
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

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
      TYPE(Base(K1,:)), POINTER :: BPtr
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(1,*)) :: Arg, IntF
      END FUNCTION
    END INTERFACE

  END MODULE

  MODULE M
  USE M0

    CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(Base(1,*)) :: Arg
    TYPE(Base(1,3)) :: ModFun
      ModFun = Arg
    END FUNCTION

    SUBROUTINE ModSub(Fun1, Fun2)
    IMPLICIT TYPE(Base(1,3))(F)
    PROCEDURE(IntF) :: Fun1
    PROCEDURE(IntF) :: Fun2
    TYPE(Base(1,3)), TARGET :: Tar=Base(1,3)("TAR", NULL())
    TYPE(Base(1,3)) :: V, U
    PROCEDURE(IntF), POINTER :: ProcPtr1
    PROCEDURE(IntF), POINTER :: ProcPtr2


    ProcPtr1 => Fun1
    IF ( .NOT. ASSOCIATED(ProcPtr1, Fun2)) STOP 71

    ProcPtr2 => ProcPtr1
    IF ( .NOT. ASSOCIATED(ProcPtr2, Fun1)) STOP 72

    V = Fun1(Base(1,3)("abc", Tar))
    IF ( V%C .NE. "abc") STOP 11
    IF ( .NOT. ASSOCIATED(V%BPtr, Tar)) STOP 13

    U = ProcPtr1(Base(1,3)("abc",Tar))
    IF ( V%C .NE. "abc") STOP 21
    IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) STOP 23

    U = Fun2(Base(1,3)("123",Tar))
    IF ( U%C .NE. "123") STOP 31
    IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) STOP 33

    U = ProcPtr2(Base(1,3)("321",Tar))
    IF ( U%C .NE. "321") STOP 31
    IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) STOP 33

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

  IF ( ABS(IntFun(SIN,1.0)-0.84147098) .GT. 1.0E-6) STOP 55
  IF ( ABS(IntFun(SQRT,4.0)-2.0) .GT. 1.0E-6)       STOP 56
  IF ( ABS(IntFun(ALOG10,10.0)-1.0) .GT. 1.0E-6)    STOP 57
  IF ( ABS(IntFun(ACOS, 0.54030231)-1.0) .GT. 1.0E-6)    STOP 58

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

