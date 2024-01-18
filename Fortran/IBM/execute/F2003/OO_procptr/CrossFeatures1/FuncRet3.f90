! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: FuncRet3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncRet3.f
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
!*  Function Return - Pointer
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
  TYPE(DT) :: Arg(:)
  TYPE(DT), POINTER :: ExtFun(:)
    !ALLOCATE(ExtFun(SIZE(Arg)), SOURCE=Arg) ! not 10.1
    ALLOCATE(ExtFun(SIZE(Arg)))
    ExtFun = Arg
  END FUNCTION

  PROGRAM FuncRet3
  USE M
  IMPLICIT TYPE(DT)(P)

  INTERFACE
    FUNCTION Fun(Arg)
      IMPORT DT
      TYPE(DT), POINTER :: Fun(:)
      TYPE(DT)          :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun)                  :: ExtFun
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


  TYPE(DT), PARAMETER :: ConstArr(3)=Const


  CALL IntSub( ExtFun(ConstArr), ConstArr)

  ProcPtr1 => ExtFun
  CALL IntSub( ProcPtr1(ConstArr),ConstArr )

  ProcPtr2 => ProcPtr1
  CALL IntSub( ProcPtr2(ConstArr),ConstArr )

  ProcPtr3 => ProcPtr2
  CALL IntSub( ProcPtr3(ConstArr),ConstArr )

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  TYPE(DT) :: Arg1(:), Arg2(:)

    DO I=1, SIZE(Arg1)

    IF (ANY(Arg1(I)%I1Arr .NE. Arg2(I)%I1Arr)) STOP 11
    IF (ANY(Arg1(I)%I2Arr .NE. Arg2(I)%I2Arr)) STOP 12
    IF (ANY(Arg1(I)%I4Arr .NE. Arg2(I)%I4Arr)) STOP 14
    IF (ANY(Arg1(I)%I8Arr .NE. Arg2(I)%I8Arr)) STOP 18

    IF (ANY(Arg1(I)%R4Arr  .NE. Arg2(I)%R4Arr))  STOP 24
    IF (ANY(Arg1(I)%R8Arr  .NE. Arg2(I)%R8Arr))  STOP 28
    IF (ANY(Arg1(I)%R16Arr .NE. Arg2(I)%R16Arr)) STOP 216

    IF (ANY(Arg1(I)%C8Arr  .NE. Arg2(I)%C8Arr))  STOP 38
    IF (ANY(Arg1(I)%C16Arr .NE. Arg2(I)%C16Arr)) STOP 316

    IF (ANY(Arg1(I)%L1Arr .NEQV. Arg2(I)%L1Arr)) STOP 41
    IF (ANY(Arg1(I)%L2Arr .NEQV. Arg2(I)%L2Arr)) STOP 42
    IF (ANY(Arg1(I)%L4Arr .NEQV. Arg2(I)%L4Arr)) STOP 44
    IF (ANY(Arg1(I)%L8Arr .NEQV. Arg2(I)%L8Arr)) STOP 48

    IF (ANY(Arg1(I)%CharArr .NE. Arg2(I)%CharArr)) STOP 68

    END DO

  END SUBROUTINE

  END

