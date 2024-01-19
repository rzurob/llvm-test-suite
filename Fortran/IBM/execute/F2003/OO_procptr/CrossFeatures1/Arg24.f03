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
  IMPLICIT TYPE(Base)(P)

    TYPE :: Base
      CHARACTER(3) :: C
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base) :: Arg(:)
        TYPE(Base) :: IntF(SIZE(Arg))
      END FUNCTION
    END INTERFACE

  CONTAINS

    SUBROUTINE ModSub1(Proc, Arr)
    IMPLICIT TYPE(Base)(P)

    TYPE(Base)      :: Arr(:)
    PROCEDURE(IntF) :: Proc
    TYPE(Base)      :: V(SIZE(Arr))

    V = Proc(Arr)
    IF (ANY(V%C .NE. Arr%C)) ERROR STOP 15

    END SUBROUTINE


    SUBROUTINE ModSub2(ProcPtr, Arr)
    IMPLICIT TYPE(Base)(P)

    TYPE(Base)               :: Arr(:)
    PROCEDURE(IntF), POINTER :: ProcPtr
    TYPE(Base)               :: V(SIZE(Arr))

    V = ProcPtr(Arr)
    IF (ANY(V%C .NE. Arr%C)) ERROR STOP 16

    END SUBROUTINE


  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base) :: Arg(:)
  TYPE(Base) :: ExtFun(SIZE(Arg))
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg24
  USE M
  IMPLICIT TYPE(Base)(P)
  PROCEDURE(IntF) :: ExtFun
  PROCEDURE(IntF), POINTER :: ProcPtr
  INTEGER :: I


  CALL ModSub1(ExtFun, (/(Base("123"), I=1,512 )/))

  ProcPtr => ExtFun
  CALL ModSub2(ProcPtr, (/(Base("321"), I=1,1024 )/))

  END

