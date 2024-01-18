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
!*  Characteristics are diff
!*  on dummy
!*  (Faile to detect-same to 304465)
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
    IntFun = 1
  END FUNCTION

  END MODULE

  PROGRAM PtrAssignCharacteristics3
  USE M
  IMPLICIT NONE

  INTERFACE
    SUBROUTINE S1(Proc, ProcPtr)
      IMPORT
      PROCEDURE(F) :: Proc
      PROCEDURE(F) :: ProcPtr
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(S1), POINTER :: ProcPtr1

  INTERFACE
    SUBROUTINE S2(Proc, ProcPtr)
      IMPORT
      INTEGER          :: Proc
      PROCEDURE(F)     :: ProcPtr
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(S2), POINTER :: ProcPtr2

  INTERFACE
    SUBROUTINE S3(Proc, ProcPtr)
      IMPORT
      PROCEDURE(F), POINTER :: Proc
      PROCEDURE(F)          :: ProcPtr
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(S3), POINTER :: ProcPtr3

  INTERFACE
    FUNCTION IFun(Proc, ProcPtr)
      IMPORT
      PROCEDURE(F), OPTIONAL :: Proc
      PROCEDURE(F), POINTER  :: ProcPtr
      INTEGER(8)             :: IFun
    END FUNCTION
  END INTERFACE

  PROCEDURE(IFun), POINTER :: ProcPtr4



  ProcPtr1 => IntSub

  ProcPtr2 => IntSub

  ProcPtr3 => IntSub

  ProcPtr4 => IntFun

  END

