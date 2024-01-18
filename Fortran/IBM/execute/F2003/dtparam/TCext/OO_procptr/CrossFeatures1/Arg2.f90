! GB DTP extension using:
! ftcx_dtp -qk -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/Arg2.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qck -qnok -qdefaultpv -qnodeferredlp -qreuse=none

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
!*  Dummy procedure - external proc/module proc
!*  ()
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


  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(4,*)) :: Arg
  TYPE(Base(4,3)) :: ExtFun
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg2
  USE M
  IMPLICIT NONE

  PROCEDURE(IntF) :: ExtFun

  CALL IntSub(ExtFun, ExtFun)

  CALL IntSub(ModFun, ModFun)

  CONTAINS

  SUBROUTINE IntSub(Fun1, Fun2)
  IMPLICIT TYPE(Base(4,3))(F)
  PROCEDURE(IntF) :: Fun1
  PROCEDURE(IntF) :: Fun2
  TYPE(Base(4,3)), TARGET :: Tar=Base(4,3)("TAR", NULL())
  TYPE(Base(4,3)) :: V, U
  PROCEDURE(IntF), POINTER :: ProcPtr1
  PROCEDURE(IntF), POINTER :: ProcPtr2


  ProcPtr1 => Fun1
  IF ( .NOT. ASSOCIATED(ProcPtr1, Fun2)) ERROR STOP 71

  ProcPtr2 => ProcPtr1
  IF ( .NOT. ASSOCIATED(ProcPtr2, Fun1)) ERROR STOP 72

  V = Fun1(Base(4,3)("abc", Tar))
  IF ( V%C .NE. "abc") ERROR STOP 11
  IF ( .NOT. ASSOCIATED(V%BPtr, Tar)) ERROR STOP 13

  U = ProcPtr1(Base(4,3)("abc",Tar))
  IF ( V%C .NE. "abc") ERROR STOP 21
  IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) ERROR STOP 23

  U = Fun2(Base(4,3)("123",Tar))
  IF ( U%C .NE. "123") ERROR STOP 31
  IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) ERROR STOP 33

  U = ProcPtr2(Base(4,3)("321",Tar))
  IF ( U%C .NE. "321") ERROR STOP 31
  IF ( .NOT. ASSOCIATED(U%BPtr, Tar)) ERROR STOP 33

  END SUBROUTINE

  END

