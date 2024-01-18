! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/Arg24.f
! opt variations: -qnock

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 26, 2005
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
!*  Argument association - array
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
    END TYPE

    IMPLICIT TYPE(Base(1,3))(P)

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(1,*)) :: Arg(:)
        TYPE(Base(1,3)) :: IntF(SIZE(Arg))
      END FUNCTION
    END INTERFACE

  CONTAINS

    SUBROUTINE ModSub1(Proc, Arr)
    IMPLICIT TYPE(Base(1,3))(P)

    TYPE(Base(1,*))      :: Arr(:)
    PROCEDURE(IntF) :: Proc
    TYPE(Base(1,3))      :: V(SIZE(Arr))

    V = Proc(Arr)
    IF (ANY(V%C .NE. Arr%C)) STOP 15

    END SUBROUTINE


    SUBROUTINE ModSub2(ProcPtr, Arr)
    IMPLICIT TYPE(Base(1,3))(P)

    TYPE(Base(1,*))               :: Arr(:)
    PROCEDURE(IntF), POINTER :: ProcPtr
    TYPE(Base(1,3))               :: V(SIZE(Arr))

    V = ProcPtr(Arr)
    IF (ANY(V%C .NE. Arr%C)) STOP 16

    END SUBROUTINE


  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(1,*)) :: Arg(:)
  TYPE(Base(1,3)) :: ExtFun(SIZE(Arg))
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg24
  USE M
  IMPLICIT TYPE(Base(1,3))(P)
  PROCEDURE(IntF) :: ExtFun
  PROCEDURE(IntF), POINTER :: ProcPtr
  INTEGER :: I


  CALL ModSub1(ExtFun, (/(Base(1,3)("123"), I=1,512 )/))

  ProcPtr => ExtFun
  CALL ModSub2(ProcPtr, (/(Base(1,3)("321"), I=1,1024 )/))

  END

