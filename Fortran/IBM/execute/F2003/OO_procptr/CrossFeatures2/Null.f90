! *********************************************************************
!*  ===================================================================
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
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(IFun), POINTER, PASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    TYPE(DT) :: Fun
    CLASS(DT) :: Arg
      Fun = Arg
    END FUNCTION

    FUNCTION IFun(Arg)
    TYPE(DT) :: IFun
    CLASS(DT) :: Arg
      IFun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null0
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V
  PROCEDURE(IFun), POINTER :: ProcPtr=>NULL()

  ProcPtr => Fun
  ProcPtr => NULL(V%ProcPtr)
  IF ( ASSOCIATED(ProcPtr) ) ERROR STOP 11

  ProcPtr => Fun
  ProcPtr => NULL(ProcPtr)
  IF ( ASSOCIATED(ProcPtr) ) ERROR STOP 12

  V%ProcPtr => Fun
  V%ProcPtr => NULL(V%ProcPtr)
  IF ( ASSOCIATED(V%ProcPtr) ) ERROR STOP 13

  ProcPtr => NULL()
  V%ProcPtr => Fun
  V%ProcPtr => NULL(ProcPtr)
  IF ( ASSOCIATED(V%ProcPtr) ) ERROR STOP 14

  ProcPtr => Fun
  V%ProcPtr => Fun
  V%ProcPtr => NULL(ProcPtr)
  IF ( ASSOCIATED(V%ProcPtr) ) ERROR STOP 15

  END


