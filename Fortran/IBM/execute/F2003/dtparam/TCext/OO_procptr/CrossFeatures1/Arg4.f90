! GB DTP extension using:
! ftcx_dtp -qk -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_procptr/CrossFeatures1/Arg4.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qck -qnok -qdefaultpv -qnodeferredlp -qreuse=self

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
!*  Dummy procedure - procedure pointer,
!*                    or a function returning a procedure pointer
!*  (304016) (306443)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(N1)             :: C
      TYPE(Base(K1,:)), POINTER :: BPtr
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(4,*)) :: Arg
        TYPE(Base(4,3)) :: IntF
      END FUNCTION
    END INTERFACE

  END MODULE

  MODULE M
  USE M0

    CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(Base(4,*)) :: Arg
    TYPE(Base(4,3)) :: ModFun
      ModFun = Arg
    END FUNCTION

    SUBROUTINE ModSub(Fun1, Fun2)
    IMPLICIT TYPE(Base(4,3))(F)
    PROCEDURE(IntF) :: Fun1
    PROCEDURE(IntF) :: Fun2
    TYPE(Base(4,3)), TARGET :: Tar=Base(4,3)("TAR", NULL())
    TYPE(Base(4,3)) :: V, U
    PROCEDURE(IntF), POINTER :: ProcPtr1
    PROCEDURE(IntF), POINTER :: ProcPtr2


    ProcPtr1 => Fun1
    IF ( .NOT. ASSOCIATED(ProcPtr1, Fun2)) STOP 71

    ProcPtr2 => ProcPtr1
    IF ( .NOT. ASSOCIATED(ProcPtr2, Fun1)) STOP 72

    V = Fun1(Base(4,3)("abc", Tar))
    IF ( V%C .NE. "abc") STOP 11
    IF ( .NOT. ASSOCIATED(V%BPtr, Tar)) STOP 13

    U = ProcPtr1(Base(4,3)("abc",Tar))
    IF ( V%C .NE. "abc") STOP 21
    IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) STOP 23

    U = Fun2(Base(4,3)("123",Tar))
    IF ( U%C .NE. "123") STOP 31
    IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) STOP 33

    U = ProcPtr2(Base(4,3)("321",Tar))
    IF ( U%C .NE. "321") STOP 31
    IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) STOP 33

    END SUBROUTINE


  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(4,*)) :: Arg
  TYPE(Base(4,3)) :: ExtFun
    ExtFun = Arg
  END FUNCTION

  FUNCTION RetPtr(Arg)
  USE M
  PROCEDURE(IntF) :: Arg
  PROCEDURE(IntF), POINTER :: RetPtr
    RetPtr => Arg
  END FUNCTION


  PROGRAM Arg4
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION RetPtr(Arg)
      IMPORT
      PROCEDURE(IntF) :: Arg
      PROCEDURE(IntF), POINTER :: RetPtr
    END FUNCTION
  END INTERFACE

  PROCEDURE(IntF), POINTER :: ProcPtr
  PROCEDURE(IntF) :: ExtFun

  ProcPtr => ExtFun
  CALL ModSub(ProcPtr, ExtFun )

  CALL ModSub(ExtFun, ExtFun )

  CALL ModSub(ExtFun, ProcPtr )

  CALL ModSub(RetPtr(ExtFun), ProcPtr )


  END

