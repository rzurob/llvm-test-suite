! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures2/Allocate2.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 9, 2005
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
!*  The allocate stmt
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(INTEGER), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION ProcFun(Arg)
    PROCEDURE(INTEGER), POINTER :: ProcFun
    PROCEDURE(INTEGER)          :: Arg
      ProcFun => Arg
    END FUNCTION

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Allocate2
  USE M
  IMPLICIT NONE
  TYPE(DT(20,4)) :: V


  ALLOCATE(ProcFun)
  ALLOCATE(ProcFun, SOURCE=Fun)
  ALLOCATE(ProcFun(Fun))
  ALLOCATE(ProcFun(Fun), SOURCE=Fun(0))

  ALLOCATE(  V%ProcPtr(Fun))
  ALLOCATE(  V%ProcPtr, SOURCE=Fun)
  ALLOCATE(  V%ProcPtr(Fun), SOURCE=V%ProcPtr(Fun))

  DEALLOCATE(V%ProcPtr(Fun))
  DEALLOCATE(ProcFun)


  END


