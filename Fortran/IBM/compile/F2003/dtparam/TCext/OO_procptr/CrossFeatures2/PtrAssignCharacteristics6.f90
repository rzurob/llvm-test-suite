! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures2/PtrAssignCharacteristics6.f
! opt variations: -qnok -ql

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
! %POSTCMD: tcomp PtrAssignCharacteristics6.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignCharacteristics6.f
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE::DT(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  CONTAINS

  FUNCTION IntFun1()
  CLASS(*), POINTER :: IntFun1
    ALLOCATE(IntFun1, SOURCE=-1)
  END FUNCTION

  FUNCTION IntFun2(Arg)
  CLASS(DT(4)), POINTER  :: IntFun2(:), Arg(:)
    ALLOCATE(IntFun2(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

  FUNCTION IntFun3()
    CLASS(DT(4)), POINTER :: IntFun3
    ALLOCATE(IntFun3, SOURCE=DT(4)())
  END FUNCTION

  FUNCTION IntFun4()
  CLASS(DT(4)), POINTER :: IntFun4
    ALLOCATE(IntFun4, SOURCE=DT(4)())
  END FUNCTION

  FUNCTION IntFun5(Arg)
  TYPE(DT(4)) :: IntFun5
  PROCEDURE(TYPE(DT(4))), POINTER :: Arg
    IntFun5 = Arg()
  END FUNCTION

  FUNCTION IntFun6(Arg)
  INTEGER :: IntFun6
  CLASS(DT(4)) :: Arg(:,:)
    IntFun6 = 1
  END FUNCTION

  FUNCTION IntFun7(Arg)
  INTEGER :: IntFun7
  CLASS(DT(4)) :: Arg(2:2)
    IntFun7 = 1
  END FUNCTION

  FUNCTION IntFun8()
  TYPE(DT(4)) :: IntFun8(3)
    IntFun8 = DT(4)()
  END FUNCTION

  FUNCTION IntFun9(M,N)
      INTEGER :: M,N
      TYPE(DT(4)) :: IntFun9(N)
    IntFun9 = DT(4)()
  END FUNCTION


  END MODULE

  PROGRAM PtrAssignCharacteristics6
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION F1()
      IMPORT DT
      CLASS(DT(4)), POINTER :: F1
    END FUNCTION
  END INTERFACE

  PROCEDURE(F1), POINTER :: ProcPtr1

  INTERFACE
    FUNCTION F2(Arg)
      IMPORT DT
      CLASS(DT(4)), POINTER :: F2(:)
      CLASS(DT(4))          :: Arg(3)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F2), POINTER :: ProcPtr2

  INTERFACE
    FUNCTION F3()
      IMPORT DT
      TYPE(DT(4)), POINTER :: F3
    END FUNCTION
  END INTERFACE

  PROCEDURE(F3), POINTER :: ProcPtr3

  INTERFACE
    FUNCTION F4()
      IMPORT DT
      CLASS(DT(4)), ALLOCATABLE :: F4
    END FUNCTION
  END INTERFACE

  PROCEDURE(F4), POINTER :: ProcPtr4

  INTERFACE
    FUNCTION F5(Arg)
      IMPORT DT
      TYPE(DT(4)) :: F5
      PROCEDURE(TYPE(DT(4))) :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(F5), POINTER :: ProcPtr5

  INTERFACE
    FUNCTION F6()
      IMPORT DT
      INTEGER :: F6
      CLASS(DT(4)), POINTER :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F6), POINTER :: ProcPtr6

  INTERFACE
    FUNCTION F7(Arg)
      IMPORT DT
      INTEGER :: F7
      CLASS(DT(4)) :: Arg(1:1, 2:2)
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

  ProcPtr4 => IntFun4

  ProcPtr5 => IntFun5

  ProcPtr6 => IntFun6

  ProcPtr7 => IntFun7

  ProcPtr8 => IntFun8

  ProcPtr9 => IntFun9

  END

