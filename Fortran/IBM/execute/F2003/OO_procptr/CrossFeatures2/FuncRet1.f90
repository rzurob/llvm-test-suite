! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 28, 2005
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
!*  Function Return - Derived types
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT

      INTEGER(1) :: I1Arr(1)
      INTEGER(2) :: I2Arr(1)
      INTEGER(4) :: I4Arr(1)
      INTEGER(8) :: I8Arr(1)

      REAL(4)  :: R4Arr(1)
      REAL(8)  :: R8Arr(1)
      REAL(16) :: R16Arr(1)

      COMPLEX(8)  :: C8Arr(1)
      COMPLEX(16) :: C16Arr(1)

      LOGICAL(1) :: L1Arr(1)
      LOGICAL(2) :: L2Arr(1)
      LOGICAL(4) :: L4Arr(1)
      LOGICAL(8) :: L8Arr(1)

      CHARACTER(3)  ::CharArr(1)

    END TYPE

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT) :: Arg
  CLASS(DT), ALLOCATABLE :: ExtFun
    ALLOCATE(ExtFun)
    SELECT TYPE (ExtFun)
    TYPE IS (DT)
      ExtFun = Arg
    CLASS DEFAULT
      STOP 88
    END SELECT
  END FUNCTION

  PROGRAM FuncRet1
  USE M
  IMPLICIT TYPE(DT)(P)

  INTERFACE
    FUNCTION Fun(Arg)
      IMPORT DT
      TYPE(DT) :: Arg
      CLASS(DT), ALLOCATABLE :: Fun
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun)                :: ExtFun
  PROCEDURE(Fun),       POINTER :: ProcPtr3

  TYPE(DT), PARAMETER ::  Const=DT(                  &
  &                                                  &
  &                            (/-1_1/),             &
  &                            (/-2_1/),             &
  &                            (/-4_1/),             &
  &                            (/-8_1/),             &
  &                                                  &
  &                            (/-4.0_4/),           &
  &                            (/-8.0_8/),           &
  &                            (/-16.0_16/),         &
  &                                                  &
  &                            (/(4.0_4,-4.0_4)/),   &
  &                            (/(8.0_8,-8.0_8)/),   &
  &                                                  &
  &                            (/.TRUE._1/),         &
  &                            (/.TRUE._2/),         &
  &                            (/.TRUE._4/),         &
  &                            (/.TRUE._8/),         &
  &                                                  &
  &                            (/"abc"/)  )





  CALL IntSub( ExtFun(Const), Const)

  ProcPtr3 => ExtFun
  CALL IntSub( ProcPtr3(Const), Const)

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  CLASS(DT) :: Arg1
  TYPE(DT)  :: Arg2

    SELECT TYPE (Arg1)
    TYPE IS (DT)

    IF (ANY(Arg1%I1Arr .NE. Arg2%I1Arr)) STOP 11
    IF (ANY(Arg1%I2Arr .NE. Arg2%I2Arr)) STOP 12
    IF (ANY(Arg1%I4Arr .NE. Arg2%I4Arr)) STOP 14
    IF (ANY(Arg1%I8Arr .NE. Arg2%I8Arr)) STOP 18

    IF (ANY(Arg1%R4Arr  .NE. Arg2%R4Arr))  STOP 24
    IF (ANY(Arg1%R8Arr  .NE. Arg2%R8Arr))  STOP 28
    IF (ANY(Arg1%R16Arr .NE. Arg2%R16Arr)) STOP 216

    IF (ANY(Arg1%C8Arr  .NE. Arg2%C8Arr))  STOP 38
    IF (ANY(Arg1%C16Arr .NE. Arg2%C16Arr)) STOP 316

    IF (ANY(Arg1%L1Arr .NEQV. Arg2%L1Arr)) STOP 41
    IF (ANY(Arg1%L2Arr .NEQV. Arg2%L2Arr)) STOP 42
    IF (ANY(Arg1%L4Arr .NEQV. Arg2%L4Arr)) STOP 44
    IF (ANY(Arg1%L8Arr .NEQV. Arg2%L8Arr)) STOP 48

    IF (ANY(Arg1%CharArr .NE. Arg2%CharArr)) STOP 68

    CLASS DEFAULT
      STOP 77
    END SELECT

  END SUBROUTINE

  END

