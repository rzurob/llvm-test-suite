! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/OO_procptr/CrossFeatures1/Assign3.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Assign3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Assign3.f
!*
!*  DATE                       : May. 16, 2005
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
!*  A derived-type intrinsic assignment
!*  (304716)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
       CHARACTER(*) :: Arg
       CHARACTER(LEN(Arg)) :: CToc
      END FUNCTION
    END INTERFACE

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT(K2)    ! (4)
      INTEGER, KIND           :: K2
      INTEGER(K2)             :: Id
      TYPE(Base(K2)), POINTER :: BComp
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE  PToP
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

    SUBROUTINE PToP (Arg1, Arg2)
    TYPE(DT(4)), INTENT (OUT) :: Arg1
    TYPE(DT(4)), INTENT (IN)  :: Arg2
      Arg1%Id  = Arg2%Id
      Arg1%BComp => Arg2%BComp
    END SUBROUTINE

  END MODULE


  PROGRAM Assign3
  USE M
  IMPLICIT NONE

  TYPE (DT(4)) :: V
  TYPE (Base(4)),     TARGET  :: BTar
  PROCEDURE(CToC), POINTER :: ProcPtr
  CHARACTER(1025)          :: Str=CHAR(40)

  ProcPtr => RetPtr(Fun)
  BTar = Base(4)(RetPtr(Fun))
  V = DT(4)(-1, BTar)

  IF ( V%Id .NE. -1 ) STOP 11
  IF ( .NOT. ASSOCIATED(V%BComp, BTar) )  STOP 12
  IF ( .NOT. ASSOCIATED(V%BComp%ProcPtr, RetPtr(Fun)) )  STOP 12

  IF (V%Bcomp%ProcPtr("ABC") .NE. "ABC" ) STOP 14

  IF (V%BComp%ProcPtr("") .NE. "" ) STOP 15

  IF (V%BComp%ProcPtr(Str) .NE. Str ) STOP 15

  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg
    RetPtr => Arg
  END FUNCTION

  END

