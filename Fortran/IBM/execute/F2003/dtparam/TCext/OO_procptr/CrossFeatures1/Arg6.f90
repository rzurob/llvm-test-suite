! GB DTP extension using:
! ftcx_dtp -qck -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/Arg6.f
! opt variations: -qnock -qdefaultpv -qdeferredlp -qreuse=none

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
!*  If an external procedure name or a dummy procedure name is used as an actual
!*  argument, its interface shall be explicit or it shall be explicitly
!*  declared to have the EXTERNAL attribute
!*
!* (304048)(305976)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND              :: K1
      INTEGER, LEN               :: N1
      CHARACTER(kind=K1,len=N1)  :: C
      TYPE(Base(K1,N1)), POINTER :: BPtr
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(1,*)) :: Arg
        TYPE(Base(1,3)), ALLOCATABLE :: IntF
      END FUNCTION
    END INTERFACE

  END MODULE

  MODULE M
  USE M0

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(1,*)) :: Arg
  TYPE(Base(1,3)), ALLOCATABLE :: ExtFun
    !ALLOCATE(ExtFun, SOURCE=Arg) ! not 10.1
    ALLOCATE(ExtFun)
    ExtFun = Arg
  END FUNCTION

  PROGRAM Arg6
  USE M
  IMPLICIT NONE
  PROCEDURE(IntF) :: ExtFun

  CALL IntSub1(ExtFun )


  CONTAINS

  SUBROUTINE IntSub1(Arg)
  PROCEDURE(IntF) :: Arg
  TYPE(Base(1,3)) :: V
  TYPE(Base(1,3)), TARGET :: Tar=Base(1,3)("abc", NULL())
    V = Arg(Base(1,3)("123", Tar))
    IF (V%C .NE. "123")                STOP 11
    IF (.NOT. ASSOCIATED(V%BPtr, Tar)) STOP 12
    IF (V%BPtr%C .NE. "abc" )          STOP 13
  END SUBROUTINE

  END

