! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Assign.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Assign.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Assign.f
!*
!*  DATE                       : May. 13, 2005
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
!*  The evaluation of expressions within variable shall neither affect
!*  nor be affected by the evaluation of expr
!*  This is a user's resposobility - Make it a normal invocation onto ptroc ptr
!*  (linux-306968)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER, INTENT(INOUT) :: Arg
    INTEGER :: Fun
      Fun = Arg
      Arg = Arg + 1
    END FUNCTION

  END MODULE


  PROGRAM Assign
  USE M
  IMPLICIT NONE

  TYPE (DT(20,4)) :: V(3)
  INTEGER   :: I
  PROCEDURE(Fun), POINTER :: ProcPtr

  ProcPtr => Fun

  I =1
  V(ProcPtr(I)) = DT(20,4)(I, ProcPtr)

  IF (I .NE. 2 )                             STOP 10
  IF (V(1)%Id .NE. 1 )                       STOP 11
  IF ( .NOT. ASSOCIATED(V(1)%ProcPtr, Fun) ) STOP 12

  I = 1
  V(I) = DT(20,4)(ProcPtr(I), ProcPtr)
  IF (I .NE. 2 )                             STOP 20
  IF (V(2)%Id .NE. 1 )                       STOP 21
  IF ( .NOT. ASSOCIATED(V(2)%ProcPtr, Fun) ) STOP 22

  END

