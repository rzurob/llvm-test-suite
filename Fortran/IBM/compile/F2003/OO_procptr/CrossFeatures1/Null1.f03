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
!*   The characteristics
!*  (err msg - 305625)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM NullTest
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V
  PROCEDURE(INTEGER),    POINTER :: ProcPtr=>NULL()
  PROCEDURE(INTEGER(2)), POINTER :: ProcPtr1=>NULL()

  INTERFACE
    INTEGER FUNCTION ExtFun()
    END FUNCTION
  END INTERFACE


  V%ProcPtr => NULL(Fun)
  ProcPtr => NULL(ExtFun)

  ProcPtr => ExtFun
  ProcPtr => NULL(ProcPtr)

  V%ProcPtr => Fun
  ProcPtr => NULL(V%ProcPtr)

  ProcPtr => ExtFun
  V%ProcPtr => NULL(ProcPtr)

  V%ProcPtr => Fun
  V%ProcPtr => NULL(V%ProcPtr)

  ProcPtr => Fun
  ProcPtr1 => NULL(ProcPtr)

  END

  INTEGER FUNCTION ExtFun()
    ExtFun = -1
  END FUNCTION
