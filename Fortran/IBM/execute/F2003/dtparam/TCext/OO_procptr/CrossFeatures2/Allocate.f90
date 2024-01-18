! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_procptr/CrossFeatures2/Allocate.f
! opt variations: -qnol -qdeferredlp

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
!*  (314716)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(ModFun), POINTER, PASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT(*,4)), TARGET  :: Arg
    CLASS(*),  POINTER :: ModFun
      ModFun => Arg
    END FUNCTION

  END MODULE

  PROGRAM Allocate0
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION IFun(Arg)
    IMPORT
      CLASS(DT(*,4)), TARGET  :: Arg
      CLASS(*), POINTER :: IFun
    END FUNCTION
  END INTERFACE

  PROCEDURE(IFun),        POINTER  :: ProcPtr
  TYPE ( DT(20,4) ),            TARGET   :: U=DT(20,4)(-1, NULL())
  CLASS(DT(20,4)), ALLOCATABLE, TARGET   :: V
  CLASS(*),  ALLOCATABLE           :: W

  ProcPtr =>  ModFun
!  ALLOCATE(V, SOURCE=ProcPtr(U))
!  IF ( .NOT. ASSOCIATED(V%ProcPtr, ModFun ) ) STOP 11
  allocate (v, source=u)
  v%procptr => procptr

  IF ( V%Id .NE. -1 )                         STOP 12


  ALLOCATE(W, SOURCE=V%ProcPtr())
  IF ( .NOT. ALLOCATED(W) ) STOP 21
  SELECT TYPE ( W)
  TYPE IS (DT(*,4))
    IF ( .NOT. ASSOCIATED(W%ProcPtr, ModFun ) ) STOP 21
    IF ( V%Id .NE. -1 )                         STOP 22
  CLASS DEFAULT
    STOP 23
  END SELECT

  END

