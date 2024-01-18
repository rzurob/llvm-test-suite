! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp PtrAssignCharacteristics2.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignCharacteristics2.f
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
!*  Characteristics are diff
!*  (304465)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  !ABSTRACT INTERFACE
  INTERFACE
    FUNCTION F(A)
      REAL :: F
      REAL :: A
    END FUNCTION
  END INTERFACE

  CONTAINS

  SUBROUTINE IntSub(Proc, ProcPtr)
  PROCEDURE(F)          :: Proc
  PROCEDURE(F), POINTER :: ProcPtr
  END SUBROUTINE

  FUNCTION IntFun(Proc, ProcPtr)
  PROCEDURE(F)          :: Proc
  PROCEDURE(F), POINTER :: ProcPtr
  INTEGER (8)           :: IntFun
    IntFun=1
  END FUNCTION

  END MODULE

  PROGRAM PtrAssignCharacteristics2
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION F1(Proc, ProcPtr)
      IMPORT
      PROCEDURE(F)          :: Proc
      PROCEDURE(F), POINTER :: ProcPtr
      INTEGER (8)           :: F1
    END FUNCTION
  END INTERFACE

  PROCEDURE(F1), POINTER :: ProcPtr1

  INTERFACE
    SUBROUTINE F2(Proc, ProcPtr)
      IMPORT
      PROCEDURE(F)          :: Proc
      PROCEDURE(F)          :: ProcPtr
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(F2), POINTER :: ProcPtr2

  INTERFACE
    FUNCTION F3(Proc, ProcPtr)
      IMPORT
      PROCEDURE(F)          :: Proc
      PROCEDURE(F), POINTER :: ProcPtr
      INTEGER (4)           :: F3
    END FUNCTION
  END INTERFACE

  PROCEDURE(F3), POINTER :: ProcPtr3



  ProcPtr1 => IntSub

  ProcPtr2 => IntSub

  ProcPtr3 => IntFun


  END

