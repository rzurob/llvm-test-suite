! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/PtrAssignCharacteristics6.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 25, 2005
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
!*  On function results: type/type parameters/defered parameters/polymorphic
!*  /pointer-allocabable/procedure pointer/rank/shape/the exact dependence of
!*  an array bound or type parameteron other entites
!*
!*  (305415) (same to 304465)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE::DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: ID
    END TYPE

  CONTAINS

  FUNCTION IntFun1()
  BYTE :: IntFun1
    IntFun1 = 1
  END FUNCTION

  FUNCTION IntFun2()
  INTEGER  :: IntFun2
    IntFun2 = 1
  END FUNCTION

  FUNCTION IntFun3()
    INTEGER :: IntFun3(3)
    IntFun3 = 1
  END FUNCTION

! FUNCTION IntFun4()
! CLASS(DT) :: IntFun4
!   IntFun4 = DT()
! END FUNCTION

  FUNCTION IntFun5()
  TYPE(DT(4)), POINTER :: IntFun5
    !ALLOCATE(IntFun5, SOURCE=DT())
    ALLOCATE(IntFun5)
    IntFun5 = DT(4)(1)
  END FUNCTION

  FUNCTION IntFun6()
  PROCEDURE(INTEGER(2)), POINTER :: IntFun6
    !IntFun6 => NULL()
    NULLIFY(IntFun6)
  END FUNCTION

  FUNCTION IntFun7()
  TYPE(DT(4)) :: IntFun7(2,2)
    IntFun7 = DT(4)(1)
  END FUNCTION

  FUNCTION IntFun8()
  TYPE(DT(4)) :: IntFun8(3)
    IntFun8 = DT(4)(1)
  END FUNCTION

  FUNCTION IntFun9(M,N)
      INTEGER :: M,N
      TYPE(DT(4)) :: IntFun9(N)
      IntFun9 = DT(4)(1)
  END FUNCTION


  END MODULE

  PROGRAM PtrAssignCharacteristics6
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION F1()
      INTEGER (1) :: F1
    END FUNCTION
  END INTERFACE

  PROCEDURE(F1), POINTER :: ProcPtr1

  INTERFACE
    FUNCTION F2()
      INTEGER (2) :: F2
    END FUNCTION
  END INTERFACE

  PROCEDURE(F2), POINTER :: ProcPtr2

  INTERFACE
    FUNCTION F3()
      INTEGER, ALLOCATABLE :: F3(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F3), POINTER :: ProcPtr3

  INTERFACE
    FUNCTION F4()
      IMPORT DT
      TYPE(DT(4)) :: F4
    END FUNCTION
  END INTERFACE

! PROCEDURE(F4), POINTER :: ProcPtr4

  INTERFACE
    FUNCTION F5()
      IMPORT DT
      TYPE(DT(4)), ALLOCATABLE :: F5
    END FUNCTION
  END INTERFACE

  PROCEDURE(F5), POINTER :: ProcPtr5

  INTERFACE
    FUNCTION F6()
      PROCEDURE(INTEGER), POINTER :: ProcPtr6
    END FUNCTION
  END INTERFACE

  PROCEDURE(F6), POINTER :: ProcPtr6

  INTERFACE
    FUNCTION F7()
      IMPORT DT
      TYPE(DT(4)) :: F7(4)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F7), POINTER :: ProcPtr7

  INTERFACE
    FUNCTION F8()
      IMPORT DT
      TYPE(DT(4)) :: F8(4)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F8), POINTER :: ProcPtr8

  INTERFACE
    FUNCTION F9(M,N)
      IMPORT DT
      INTEGER :: M,N
      TYPE(DT(4)) :: F9(M)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F9), POINTER :: ProcPtr9


  ProcPtr1 => IntFun1

  ProcPtr2 => IntFun2

  ProcPtr3 => IntFun3

! ProcPtr4 => IntFun4  ! not in 10.1

  ProcPtr5 => IntFun5

  ProcPtr6 => IntFun6

  ProcPtr7 => IntFun7

  ProcPtr8 => IntFun8

  ProcPtr9 => IntFun9

  END

