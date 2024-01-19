! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 9, 2005
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
!*  The allocate stmt
!*
!*  (304081)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(INTEGER), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION ModFun()
    INTEGER ModFun
      Modfun = -1
    END FUNCTION

  END MODULE

  PROGRAM Allocate
  USE M
  IMPLICIT NONE

  PROCEDURE(TYPE(DT)), POINTER :: ProcPtr=>NULL()
  TYPE ( DT ),         POINTER :: V
  PROCEDURE(TYPE(DT))          :: Fun

  ProcPtr => Fun

  !ALLOCATE(V, SOURCE=ProcPtr(DT(-1, ModFun))) ! not 10.1
  ALLOCATE(V)
  V = ProcPtr(DT(-1, ModFun))
  IF ( .NOT. ASSOCIATED(V) )                 ERROR STOP 11
  IF ( V%Id .NE. -1 )                        ERROR STOP 12
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ModFun) ) ERROR STOP 13

  DEALLOCATE(V)

  END

  FUNCTION Fun(Arg)
  USE M
  TYPE (DT) :: Fun
  TYPE(DT)  :: Arg
    Fun = Arg
  END FUNCTION

