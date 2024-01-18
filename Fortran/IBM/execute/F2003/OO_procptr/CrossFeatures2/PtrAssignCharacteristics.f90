! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 18, 2005
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
!*  proc-pointer-object is not pure while proc-target may be pure
!*  (315181)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PURE FUNCTION ExtFun(Arg)
  CLASS(*), POINTER    :: ExtFun
  CLASS(*), INTENT(IN) :: Arg
    ALLOCATE(ExtFun, SOURCE=Arg)
  END FUNCTION

  PROGRAM PtrAssignCharacteristics
  IMPLICIT NONE

  TYPE :: DT
    CLASS(*), ALLOCATABLE :: V
  END TYPE

  INTERFACE
    FUNCTION ExtF1(Arg)
      CLASS(*), POINTER    :: ExtF1
      CLASS(*), INTENT(IN) :: Arg
    END FUNCTION
  END INTERFACE

  INTERFACE
    PURE FUNCTION ExtF2(Arg)
      CLASS(*), POINTER    :: ExtF2
      CLASS(*), INTENT(IN) :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(ExtF1), POINTER :: ProcPtr
  PROCEDURE(ExtF2)          :: ExtFun

  INTEGER :: i

  ProcPtr => ExtFun

  IF ( .NOT. ASSOCIATED(ProcPtr) )          STOP 21
  IF ( .NOT. ASSOCIATED(ProcPtr, ExtFun) )  STOP 22
  IF ( .NOT. ASSOCIATED(ProcPtr, ProcPtr))  STOP 23

  SELECT TYPE ( As => ProcPtr(DT(DT(12345678_8)) ))
  TYPE IS (DT)
    SELECT TYPE ( As => As%V)
    TYPE IS (DT)
      SELECT TYPE ( As => As%V)
      TYPE IS (INTEGER(8))
        IF ( As .NE. 12345678_8 )    STOP 31
      CLASS DEFAULT
        STOP 32
      END SELECT
    CLASS DEFAULT
      STOP 33
    END SELECT
  CLASS DEFAULT
    STOP 34
  END SELECT

  END

