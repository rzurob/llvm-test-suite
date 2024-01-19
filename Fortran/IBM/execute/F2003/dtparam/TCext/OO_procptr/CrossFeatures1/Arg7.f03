! GB DTP extension using:
! ftcx_dtp -qk -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/Arg7.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qck -qnok -qdefaultpv -qnodeferredlp -qreuse=none

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
!*  Implicit dummy procedure
!*
!* (syntax and ice)
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
        TYPE(Base(4,3)):: IntF
      END FUNCTION
    END INTERFACE

  END MODULE

  MODULE M
  USE M0

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(4,*)) :: Arg
  TYPE(Base(4,3)) :: ExtFun
    ExtFun = Arg
  END FUNCTION

  FUNCTION ExtFun1(Arg)
  USE M
  TYPE(Base(4,*)) :: Arg
  TYPE(Base(4,3)) :: ExtFun1
    ExtFun1 = Arg
  END FUNCTION


  PROGRAM Arg7
  USE M
  IMPLICIT NONE
  PROCEDURE(IntF) :: ExtFun
  PROCEDURE(IntF) :: ExtFun1

  CALL Intsub(ExtFun )
  CALL Intsub(ExtFun1)


  CONTAINS

    SUBROUTINE IntSub(Arg)
    PROCEDURE(IntF)         :: Arg
    TYPE(Base(4,3))         :: V
    TYPE(Base(4,3)), TARGET :: Tar=Base(4,3)("abc", NULL())
      V = Arg(Base(4,3)("123", Tar))
      IF (V%C .NE. "123")                ERROR STOP 11
      IF (.NOT. ASSOCIATED(V%BPtr, Tar)) ERROR STOP 12
      IF (V%BPtr%C .NE. "abc" )          ERROR STOP 13
    END SUBROUTINE

  END

