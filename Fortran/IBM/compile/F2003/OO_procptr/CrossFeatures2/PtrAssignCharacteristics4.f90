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
!*  on dummy data objects:
!*  type/type parameters/shape/bounds/intent/optioal/pointer/allocatable/
!*  value/asynchronous/volatile/polymorphic/assume on shape-size-type parameter
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: Base
  END TYPE

  TYPE, EXTENDS(Base) :: DT
  END TYPE

  CONTAINS

  FUNCTION IntFun1(Arg)
  INTEGER (4) :: IntFun1
  INTEGER (8) :: Arg
    IntFun1 = Arg
  END FUNCTION

  FUNCTION IntFun2(Arg)
  INTEGER (4) :: IntFun2
  INTEGER (4) :: Arg(3)
    IntFun2 = Arg(1)
  END FUNCTION

  FUNCTION IntFun3(Arg)
  INTEGER (4) :: IntFun3
  INTEGER (4) :: Arg(3,3)
    IntFun3 = Arg(1,1)
  END FUNCTION

  FUNCTION IntFun4(Arg)
  INTEGER (4) :: IntFun4
  INTEGER (4), INTENT(INOUT) :: Arg(3,3)
    IntFun4 = Arg(1,1)
  END FUNCTION

  FUNCTION IntFun5(Arg)
  INTEGER (4) :: IntFun5
  INTEGER (4) :: Arg(3,3)
    IntFun5 = Arg(1,1)
  END FUNCTION

  FUNCTION IntFun6(Arg)
  INTEGER (4) :: IntFun6
  INTEGER (4) :: Arg(3,3)
    IntFun6 = Arg(1,1)
  END FUNCTION

  FUNCTION IntFun7(Arg)
  INTEGER (4) :: IntFun7
  INTEGER (4), ALLOCATABLE :: Arg(:,:)
    ALLOCATE(Arg(3,3), SOURCE=1)
    IntFun7 = Arg(1,1)
  END FUNCTION

  FUNCTION IntFun8(Arg)
  INTEGER (4) :: IntFun8
  INTEGER (4), VALUE :: Arg
    IntFun8 = Arg
  END FUNCTION

  FUNCTION IntFun9(Arg)
  INTEGER (4) :: IntFun9
  INTEGER (4) :: Arg(3,3)
    IntFun9 = Arg(1,1)
  END FUNCTION

  FUNCTION IntFun10(Arg)
  INTEGER (4) :: IntFun10
  INTEGER (4), VOLATILE :: Arg(3,3)
    IntFun10 = Arg(1,1)
  END FUNCTION

  FUNCTION IntFun11(Arg)
  INTEGER (4) :: IntFun11
  CLASS(BASE) :: Arg(3)
    IntFun11 = 1
  END FUNCTION

  FUNCTION IntFun12(Arg)
  INTEGER (4) :: IntFun12
  INTEGER (4) :: Arg(3,3)
    IntFun12 = Arg(1,1)
  END FUNCTION


  END MODULE

  PROGRAM PtrAssignCharacteristics4
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION F1(Arg)
      INTEGER (4) :: F1
      INTEGER (4) :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(F1), POINTER :: ProcPtr1

  INTERFACE
    FUNCTION F2(Arg)
      INTEGER (4) :: F2
      INTEGER (4) :: Arg(2)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F2), POINTER :: ProcPtr2

  INTERFACE
    FUNCTION F3(Arg)
      INTEGER (4) :: F3
      INTEGER (4) :: Arg(3)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F3), POINTER :: ProcPtr3

  INTERFACE
    FUNCTION F4(Arg)
      INTEGER (4) :: F4
      INTEGER (4) :: Arg(3)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F4), POINTER :: ProcPtr4

  INTERFACE
    FUNCTION F5(Arg)
      INTEGER (4) :: F5
      INTEGER (4), OPTIONAL :: Arg(3,3)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F5), POINTER :: ProcPtr5

  INTERFACE
    FUNCTION F6(Arg)
      INTEGER (4) :: F6
      INTEGER (4), POINTER :: Arg(:,:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F6), POINTER :: ProcPtr6

  INTERFACE
    FUNCTION F7(Arg)
      INTEGER (4) :: F7
      INTEGER (4), POINTER :: Arg(:,:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F7), POINTER :: ProcPtr7

  INTERFACE
    FUNCTION F8(Arg)
      INTEGER (4) :: F8
      INTEGER (4) :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(F8), POINTER :: ProcPtr8

! Not implemented yet
! INTERFACE
!   FUNCTION F9(Arg)
!     INTEGER (4) :: F9
!     INTEGER (4), ASYNCHRONOUS :: Arg(3,3)
!   END FUNCTION
! END INTERFACE

! PROCEDURE(F9), POINTER :: ProcPtr9

  INTERFACE
    FUNCTION F10(Arg)
      INTEGER (4) :: F10
      INTEGER (4) :: Arg(3,3)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F10), POINTER :: ProcPtr10

  INTERFACE
    FUNCTION F11(Arg)
    IMPORT DT
      INTEGER (4) :: F11
      CLASS(DT) :: Arg(3,3)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F11), POINTER :: ProcPtr11

  INTERFACE
    FUNCTION F12(Arg)
      INTEGER (4) :: F12
      INTEGER (4) :: Arg(3,*)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F12), POINTER :: ProcPtr12



  ProcPtr1 => IntFun1

  ProcPtr2 => IntFun2

  ProcPtr3 => IntFun3

  ProcPtr4 => IntFun4

  ProcPtr5 => IntFun5

  ProcPtr6 => IntFun6

  ProcPtr7 => IntFun7

  ProcPtr8 => IntFun8

! ProcPtr9 => IntFun9

  ProcPtr10 => IntFun10

  ProcPtr11 => IntFun11

  ProcPtr12 => IntFun12

  END

