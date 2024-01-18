! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures2/Null.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Null.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Null.f
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
!*   null()
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(IFun), POINTER, PASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    TYPE(DT(20,4)) :: Fun
    CLASS(DT(*,4)) :: Arg
      Fun = Arg
    END FUNCTION

    FUNCTION IFun(Arg)
    TYPE(DT(20,4)) :: IFun
    CLASS(DT(*,4)) :: Arg
      IFun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null0
  USE M
  IMPLICIT NONE

  TYPE (DT(20,4)) :: V
  PROCEDURE(IFun), POINTER :: ProcPtr=>NULL()

  ProcPtr => Fun
  ProcPtr => NULL(V%ProcPtr)
  IF ( ASSOCIATED(ProcPtr) ) STOP 11

  ProcPtr => Fun
  ProcPtr => NULL(ProcPtr)
  IF ( ASSOCIATED(ProcPtr) ) STOP 12

  V%ProcPtr => Fun
  V%ProcPtr => NULL(V%ProcPtr)
  IF ( ASSOCIATED(V%ProcPtr) ) STOP 13

  ProcPtr => NULL()
  V%ProcPtr => Fun
  V%ProcPtr => NULL(ProcPtr)
  IF ( ASSOCIATED(V%ProcPtr) ) STOP 14

  ProcPtr => Fun
  V%ProcPtr => Fun
  V%ProcPtr => NULL(ProcPtr)
  IF ( ASSOCIATED(V%ProcPtr) ) STOP 15

  END


