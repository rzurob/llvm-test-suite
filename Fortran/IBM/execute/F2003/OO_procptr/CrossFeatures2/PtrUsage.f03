! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 20, 2005
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
!*  Usage: Pass procedure itself as target / argument
!*
!*  (315457/316472)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  LOGICAL :: L(3) = .FALSE.

  CONTAINS

  RECURSIVE SUBROUTINE ModSub()
    CALL ModSub1(ModSub)
    L(3) = .TRUE.
  END SUBROUTINE

  SUBROUTINE ModSub1(Arg)
  PROCEDURE() :: Arg
  PROCEDURE(), POINTER :: ProcPtr
  PROCEDURE(ModSub1), POINTER :: ProcPtr1

    ProcPtr => Arg
    IF ( .NOT. ASSOCIATED(ProcPtr, ModSub)) ERROR STOP 10

    ProcPtr1 => ModSub1
    CAll ModSub2(ProcPtr1)
    L(2) = .TRUE.

  END SUBROUTINE

  SUBROUTINE ModSub2(ProcPtr)
  PROCEDURE(ModSub1), POINTER :: ProcPtr
    IF ( .NOT. ASSOCIATED(ProcPtr, ModSub1)) ERROR STOP 11
    L(1) = .TRUE.
  END SUBROUTINE

  END MODULE


  PROGRAM PtrUsage
  USE M
  IMPLICIT NONE

  CALL ModSub()
  IF ( .NOT. ANY(L) ) ERROR STOP 12

  END
