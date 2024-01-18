! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 17, 2005
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
!*  Defined assignment
!*  (304718) -> 305334
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
       CHARACTER(*) :: Arg
       CHARACTER(LEN(Arg)) :: CToC
      END FUNCTION
    END INTERFACE

    TYPE :: Base
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT
      INTEGER :: Id=0
      TYPE(Base), POINTER :: BComp
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE MyAssign1
!     MODULE PROCEDURE MyAssign2
      MODULE PROCEDURE MyAssign3
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

    SUBROUTINE MyAssign1 (Arg1, Arg2)
    PROCEDURE(CToC), POINTER, INTENT(OUT) :: Arg1
    PROCEDURE(CToC), POINTER, INTENT(IN) :: Arg2
      Arg1 => Arg2
    END SUBROUTINE

!   SUBROUTINE MyAssign2 (Arg1, Arg2)
!   PROCEDURE(CToC), POINTER, INTENT(OUT) :: Arg1
!   PROCEDURE(CToC), INTENT(IN) :: Arg2 !disallowed by C1214
!     Arg1 => Arg2
!   END SUBROUTINE

    SUBROUTINE MyAssign3 (Arg1, Arg2)
    PROCEDURE(CToC), POINTER, INTENT(OUT) :: Arg1
    TYPE(DT), INTENT(IN) :: Arg2
      Arg1 => Arg2%BComp%ProcPtr
    END SUBROUTINE

  END MODULE


  PROGRAM DefAssign
  USE M
  IMPLICIT NONE

  PROCEDURE(CToC), POINTER :: ProcPtr
  PROCEDURE(CToC), POINTER :: ProcPtr1
  TYPE(Base),      TARGET  :: BTar

  ProcPtr => NULL()
  ProcPtr = RetPtr(Fun)
  IF (ProcPtr("ABC") .NE. "ABC" ) ERROR STOP 14

  ProcPtr => Fun
  ProcPtr = NULL(ProcPtr)
  IF (ASSOCIATED(ProcPtr) ) ERROR STOP 15

  ProcPtr1 => Fun
  ProcPtr = ProcPtr1
  IF (ProcPtr("0123456789") .NE. "0123456789" ) ERROR STOP 16

  ProcPtr => NULL()
  BTar =  Base( Fun)
  ProcPtr = DT(-1, BTar)
  IF (ProcPtr("xyz") .NE. "xyz" ) ERROR STOP 17

  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg
    RetPtr => Arg
  END FUNCTION

  END

