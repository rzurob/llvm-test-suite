! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures2/Data.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Data.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Data.f
!*
!*  DATE                       : May. 12, 2005
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
!*  Var shall not be explicitly initialized more than once
!*
!*  (314850)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), POINTER :: ProcPtr
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    CLASS(DT(*,4)) :: Arg
    CLASS(*), ALLOCATABLE :: Fun
      ALLOCATE(Fun, SOURCE=Arg)
    END FUNCTION

  END MODULE


  PROGRAM Data
  USE M
  IMPLICIT NONE

  PROCEDURE(Fun), POINTER :: ProcPtr=>NULL()
  DATA ProcPtr /NULL(ProcPtr)/

  PROCEDURE(Fun), POINTER :: ProcPtr1, ProcPtr2
  DATA ProcPtr1 /NULL(ProcPtr)/, ProcPtr1 /NULL()/
  DATA ProcPtr2, ProcPtr2 /2*NULL()/

  TYPE (DT(20,4)) :: V, V1(3), V2(3)
  DATA V /DT(20,4)(-1, NULL())/
  DATA V1 /3*DT(20,4)(-1, NULL())/
  DATA V2(2:3) /2*DT(20,4)(-1, NULL())/

  TYPE (DT(20,4)) :: W, W1(3), W2(3)
  DATA W%ProcPtr / NULL()/
  DATA W1(1)%ProcPtr, W1(1)%ProcPtr  /2*NULL()/  !?

  END


