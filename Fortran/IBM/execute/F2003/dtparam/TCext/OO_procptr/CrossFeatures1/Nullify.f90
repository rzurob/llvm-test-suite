! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Nullify.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Nullify.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Nullify.f
!*
!*  DATE                       : May. 10, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  The nullify stmt
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    CHARACTER(10) :: Mark

    CONTAINS

    FUNCTION ProcFun(Arg)
    PROCEDURE(TYPE(DT(20,4))), POINTER :: ProcFun
    PROCEDURE(TYPE(DT(20,4)))          :: Arg
      ProcFun => Arg
    END FUNCTION

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Nullify
  USE M
  IMPLICIT NONE

  TYPE(DT(20,4))                :: V
  PROCEDURE(Fun), POINTER :: ProcPtr1=>NULL()
  PROCEDURE(   ), POINTER :: ProcPtr2=>NULL()
  EXTERNAL                :: ExtSub

  V%ProcPtr =>  Fun
  IF ( .NOT. ASSOCIATED( V%ProcPtr )) STOP 11
  IF (  V%ProcPtr(-1) .NE. -1)        STOP 12
  NULLIFY(V%ProcPtr)
  IF ( ASSOCIATED( V%ProcPtr ) )     STOP 13

  ProcPtr1 =>  Fun
  IF ( .NOT. ASSOCIATED( ProcPtr1 )) STOP 21
  IF (  ProcPtr1(-2) .NE. -2)        STOP 22
  NULLIFY(ProcPtr1)
  IF ( ASSOCIATED( ProcPtr1 ))       STOP 23

  ProcPtr2 => ExtSub
  IF ( .NOT. ASSOCIATED( ProcPtr2 )) STOP 31
  CALL ProcPtr2("-3")
  IF ( TRIM(Mark) .NE. "-3")         STOP 32
  NULLIFY(ProcPtr2)
  IF ( ASSOCIATED( ProcPtr2 ))       STOP 33

  V%ProcPtr =>  Fun
  ProcPtr1 =>  Fun
  ProcPtr1 => ProcPtr1
  IF ( .NOT. ASSOCIATED( V%ProcPtr )) STOP 41
  IF ( .NOT. ASSOCIATED( ProcPtr1 ))  STOP 42
  IF (  V%ProcPtr(-1) .NE. -1)        STOP 43
  IF (  ProcPtr1(-2)   .NE. -2)       STOP 44
  NULLIFY(V%ProcPtr, ProcPtr1)
  IF ( ASSOCIATED( V%ProcPtr ))       STOP 45
  IF ( ASSOCIATED( ProcPtr1 ))        STOP 46


  END

  SUBROUTINE ExtSub(Arg)
  USE M
  IMPLICIT NONE
  CHARACTER(*) :: Arg
    Mark = Arg
  END SUBROUTINE
