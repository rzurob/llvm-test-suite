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
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(ModFun), POINTER, PASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT), TARGET  :: Arg
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
      CLASS(DT), TARGET  :: Arg
      CLASS(*), POINTER :: IFun
    END FUNCTION
  END INTERFACE

  PROCEDURE(IFun),        POINTER  :: ProcPtr
  TYPE ( DT ),            TARGET   :: U=DT(-1, NULL())
  CLASS(DT), ALLOCATABLE, TARGET   :: V
  CLASS(*),  ALLOCATABLE           :: W

  ProcPtr =>  ModFun
!  ALLOCATE(V, SOURCE=ProcPtr(U))
!  IF ( .NOT. ASSOCIATED(V%ProcPtr, ModFun ) ) ERROR STOP 11
  allocate (v, source=u)
  v%procptr => procptr
  IF ( V%Id .NE. -1 )                         ERROR STOP 12

  ALLOCATE(W, SOURCE=V%ProcPtr())
  IF ( .NOT. ALLOCATED(W) ) ERROR STOP 21
  SELECT TYPE ( W)
  TYPE IS (DT)
    IF ( .NOT. ASSOCIATED(W%ProcPtr, ModFun ) ) ERROR STOP 21
    IF ( V%Id .NE. -1 )                         ERROR STOP 22
  CLASS DEFAULT
    STOP 23
  END SELECT

  END

