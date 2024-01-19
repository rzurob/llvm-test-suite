!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Generalization of PROCEDURE statement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 296676
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C1207 (R1206) A procedure-name shall have an explicit interface and shall
!*  refer to an accessible procedure pointer, external procedure,
!*  dummy procedure, or module procedure.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM mProcC1207_1

  PROCEDURE() :: Sub
  INTERFACE Fun
    PROCEDURE Sub
  END INTERFACE

  PROCEDURE(INTEGER) Fun1
  INTERFACE Fun
    PROCEDURE Fun1
  END INTERFACE

  External ExtSub
  INTERFACE Fun
    PROCEDURE ExtSub
  END INTERFACE

  INTERFACE Fun
    PROCEDURE ExtSub1
  END INTERFACE
  CALL ExtSub1()

  END

  MODULE M

  CONTAINS

  RECURSIVE FUNCTION F() !RESULT(R)
  INTEGER :: F
    INTERFACE Fun
      PROCEDURE F
    END INTERFACE
    F = 1
  END FUNCTION

  END MODULE

  ! This is fine
  RECURSIVE SUBROUTINE ExtSub()
    INTERFACE Fun
      PROCEDURE ExtSub
    END INTERFACE
  END SUBROUTINE


