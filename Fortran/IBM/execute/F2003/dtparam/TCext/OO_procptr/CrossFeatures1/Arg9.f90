! GB DTP extension using:
! ftcx_dtp -qck -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/Arg9.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnock -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 23, 2005
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
!*  If an external procedure name or a dummy procedure name is used as an actual
!*  argument, its interface shall be explicit or it shall be explicitly
!*  declared to have the EXTERNAL attribute-
!*  External
!*
!* (Comp failed)
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
        TYPE(Base(1,*)) :: Arg
        TYPE(Base(1,3)):: IntF
      END FUNCTION
    END INTERFACE

  END MODULE

  MODULE M
  USE M0

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(1,*)) :: Arg
  TYPE(Base(1,3)) :: ExtFun
    ExtFun = Arg
  END FUNCTION

  FUNCTION ExtFun1(Arg)
  USE M
  TYPE(Base(1,*)) :: Arg
  TYPE(Base(1,3)) :: ExtFun1
    ExtFun1 = Arg
  END FUNCTION


  PROGRAM Arg9
  USE M
  IMPLICIT NONE

  PROCEDURE(IntF) :: ExtFun
  PROCEDURE(IntF) :: ExtFun1

  CALL Intsub(ExtFun )
  CALL Intsub(ExtFun1)

  CALL IntSub1(extFun)
  CALL IntSub1(extFun1)

  CALL IntSub2(extFun)
  CALL IntSub2(extFun1)

  CONTAINS

    SUBROUTINE IntSub(Arg)
    IMPLICIT TYPE(Base(1,3))(A)
    PROCEDURE(IntF)         :: Arg
    TYPE(Base(1,3))         :: V
    TYPE(Base(1,3)), TARGET :: Tar=Base(1,3)("abc", NULL())
      V = Arg(Base(1,3)("123", Tar))
      IF (V%C .NE. "123")                STOP 11
      IF (.NOT. ASSOCIATED(V%BPtr, Tar)) STOP 12
      IF (V%BPtr%C .NE. "abc" )          STOP 13
    END SUBROUTINE

    SUBROUTINE IntSub1(Arg)
    IMPLICIT TYPE(Base(1,3))(A)
    ! PROCEDURE() :: Arg  ! This is causing trouble
    PROCEDURE(IntF) :: Arg
      CALL IntSub(Arg)
    END SUBROUTINE

    SUBROUTINE IntSub2(Arg)
    PROCEDURE(IntF) :: Arg
      CALL IntSub(Arg)
    END SUBROUTINE

  END

