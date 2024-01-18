! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: FuncRet6.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncRet6.f
!*
!*  DATE                       : May. 27, 2005
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
!*  Function Return - entry stmt
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

    TYPE :: DT1
      TYPE(DT), ALLOCATABLE :: T1
      TYPE(DT), POINTER     :: T2
    END TYPE

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT1) :: Arg(:)
  TYPE(DT1), POINTER :: ExtFun(:)
  TYPE(DT1), POINTER :: ExtFun1(:), ExtFun2(:)

    !ALLOCATE(ExtFun(SIZE(Arg)), SOURCE=Arg)
    ALLOCATE(ExtFun(SIZE(Arg)))
    ExtFun = Arg
    RETURN

  ENTRY ExtFun1(Arg)
    !ALLOCATE(ExtFun1(SIZE(Arg)), SOURCE=Arg)
    ALLOCATE(ExtFun1(SIZE(Arg)))
    ExtFun1 = Arg
    RETURN

  ENTRY ExtFun2(Arg)
    !ALLOCATE(ExtFun2(SIZE(Arg)), SOURCE=Arg)
    ALLOCATE(ExtFun2(SIZE(Arg)))
    ExtFun2 = Arg
    RETURN

  END FUNCTION

  PROGRAM FuncRet6
  USE M
  IMPLICIT TYPE(DT1)(P)

  INTERFACE
    FUNCTION Fun(Arg)
      IMPORT DT1
      TYPE(DT1), POINTER :: Fun(:)
      TYPE(DT1)          :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun)                  :: ExtFun
  PROCEDURE(Fun)                  :: ExtFun1
  PROCEDURE(Fun)                  :: ExtFun2
  PROCEDURE(Fun),         POINTER :: ProcPtr1
  PROCEDURE(ExtFun),      POINTER :: ProcPtr2
  PROCEDURE(ProcPtr2),    POINTER :: ProcPtr3

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


  TYPE(DT1)        :: ConstArr(10000)
  TYPE(DT), TARGET :: Tar=Const

  ConstArr = DT1(Const, Tar)

  CALL IntSub( ExtFun(ConstArr), ConstArr)

  ProcPtr1 => ExtFun
  CALL IntSub( ProcPtr1(ConstArr),ConstArr )

  ProcPtr1 => ExtFun1
  CALL IntSub( ProcPtr1(ConstArr),ConstArr )

  ProcPtr1 => ExtFun2
  CALL IntSub( ProcPtr1(ConstArr),ConstArr )

  ProcPtr2 => ProcPtr1
  CALL IntSub( ProcPtr2(ConstArr),ConstArr )

  ProcPtr3 => ProcPtr2
  CALL IntSub( ProcPtr3(ConstArr),ConstArr )

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  TYPE(DT1) :: Arg1(:), Arg2(:)

    DO I=1, SIZE(Arg1)

    IF (ANY(Arg1(I)%T1%I1Arr .NE. Arg2(I)%T1%I1Arr)) STOP 11
    IF (ANY(Arg1(I)%T1%I2Arr .NE. Arg2(I)%T1%I2Arr)) STOP 12
    IF (ANY(Arg1(I)%T1%I4Arr .NE. Arg2(I)%T1%I4Arr)) STOP 14
    IF (ANY(Arg1(I)%T1%I8Arr .NE. Arg2(I)%T1%I8Arr)) STOP 18

    IF (ANY(Arg1(I)%T1%R4Arr  .NE. Arg2(I)%T1%R4Arr))  STOP 24
    IF (ANY(Arg1(I)%T1%R8Arr  .NE. Arg2(I)%T1%R8Arr))  STOP 28
    IF (ANY(Arg1(I)%T1%R16Arr .NE. Arg2(I)%T1%R16Arr)) STOP 216

    IF (ANY(Arg1(I)%T1%C8Arr  .NE. Arg2(I)%T1%C8Arr))  STOP 38
    IF (ANY(Arg1(I)%T1%C16Arr .NE. Arg2(I)%T1%C16Arr)) STOP 316

    IF (ANY(Arg1(I)%T1%L1Arr .NEQV. Arg2(I)%T1%L1Arr)) STOP 41
    IF (ANY(Arg1(I)%T1%L2Arr .NEQV. Arg2(I)%T1%L2Arr)) STOP 42
    IF (ANY(Arg1(I)%T1%L4Arr .NEQV. Arg2(I)%T1%L4Arr)) STOP 44
    IF (ANY(Arg1(I)%T1%L8Arr .NEQV. Arg2(I)%T1%L8Arr)) STOP 48

    IF (ANY(Arg1(I)%T1%CharArr .NE. Arg2(I)%T1%CharArr)) STOP 68

    END DO

    DO I=1, SIZE(Arg1)

    IF (ANY(Arg1(I)%T2%I1Arr .NE. Arg2(I)%T2%I1Arr)) STOP 11
    IF (ANY(Arg1(I)%T2%I2Arr .NE. Arg2(I)%T2%I2Arr)) STOP 12
    IF (ANY(Arg1(I)%T2%I4Arr .NE. Arg2(I)%T2%I4Arr)) STOP 14
    IF (ANY(Arg1(I)%T2%I8Arr .NE. Arg2(I)%T2%I8Arr)) STOP 18

    IF (ANY(Arg1(I)%T2%R4Arr  .NE. Arg2(I)%T2%R4Arr))  STOP 24
    IF (ANY(Arg1(I)%T2%R8Arr  .NE. Arg2(I)%T2%R8Arr))  STOP 28
    IF (ANY(Arg1(I)%T2%R16Arr .NE. Arg2(I)%T2%R16Arr)) STOP 216

    IF (ANY(Arg1(I)%T2%C8Arr  .NE. Arg2(I)%T2%C8Arr))  STOP 38
    IF (ANY(Arg1(I)%T2%C16Arr .NE. Arg2(I)%T2%C16Arr)) STOP 316

    IF (ANY(Arg1(I)%T2%L1Arr .NEQV. Arg2(I)%T2%L1Arr)) STOP 41
    IF (ANY(Arg1(I)%T2%L2Arr .NEQV. Arg2(I)%T2%L2Arr)) STOP 42
    IF (ANY(Arg1(I)%T2%L4Arr .NEQV. Arg2(I)%T2%L4Arr)) STOP 44
    IF (ANY(Arg1(I)%T2%L8Arr .NEQV. Arg2(I)%T2%L8Arr)) STOP 48

    IF (ANY(Arg1(I)%T2%CharArr .NE. Arg2(I)%T2%CharArr)) STOP 68

    END DO

  END SUBROUTINE

  END

