! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 22, 2005
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
!*  dummy procedure/procedure pointer, explicit/implict interface,
!*  characteristics of the interface, or optional
!*  (same to 304465)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  !ABSTRACT INTERFACE
  INTERFACE
    FUNCTION F0(Arg)
      INTEGER (4) :: F0
      INTEGER (4) :: Arg
    END FUNCTION
  END INTERFACE

  !ABSTRACT INTERFACE
  INTERFACE
    FUNCTION F00(Arg)
      INTEGER (8) :: F00
      INTEGER (4) :: Arg
    END FUNCTION
  END INTERFACE

  CONTAINS

  FUNCTION IntFun1(Arg)
  INTEGER (4) :: IntFun1
  PROCEDURE(REAL), POINTER :: Arg
    IntFun1 = Arg()
  END FUNCTION

  FUNCTION IntFun2(Arg)
  INTEGER (4) :: IntFun2
  PROCEDURE(REAL) :: Arg
    IntFun2 = Arg(1)
  END FUNCTION

  FUNCTION IntFun3(Arg)
  INTEGER (4) :: IntFun3
  PROCEDURE(INTEGER (4)), POINTER :: Arg
    IntFun3 = Arg(1)
  END FUNCTION

  FUNCTION IntFun4(Arg)
  INTEGER (4) :: IntFun4
  PROCEDURE(F00), POINTER :: Arg
    IntFun4 = Arg(1)
  END FUNCTION

  FUNCTION IntFun5(Arg)
  INTEGER (4) :: IntFun5
  PROCEDURE(F0), POINTER :: Arg
    IntFun5 = Arg(1)
  END FUNCTION

  END MODULE


  PROGRAM PtrAssignCharacteristics5
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION F1(Arg)
      INTEGER (4) :: F1
      PROCEDURE(REAL) :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(F1), POINTER :: ProcPtr1

  INTERFACE
    FUNCTION F2(Arg)
      INTEGER (4) :: F2
      PROCEDURE(REAL), POINTER :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(F2), POINTER :: ProcPtr2

  INTERFACE
    FUNCTION F3(Arg)
      IMPORT
      INTEGER (4) :: F3
      PROCEDURE(F0), POINTER :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(F3), POINTER :: ProcPtr3

  INTERFACE
    FUNCTION F4(Arg)
      IMPORT
      INTEGER (4) :: F4
      PROCEDURE(F0), POINTER :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(F4), POINTER :: ProcPtr4

  INTERFACE
    FUNCTION F5(Arg)
      IMPORT
      INTEGER (4) :: F5
      PROCEDURE(F0), POINTER, OPTIONAL :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(F5), POINTER :: ProcPtr5


  ProcPtr1 => IntFun1

  ProcPtr2 => IntFun2

  ProcPtr3 => IntFun3

  ProcPtr4 => IntFun4

  ProcPtr5 => IntFun5

  END

