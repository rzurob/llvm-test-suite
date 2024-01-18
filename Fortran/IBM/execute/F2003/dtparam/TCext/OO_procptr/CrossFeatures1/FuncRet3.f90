! GB DTP extension using:
! ftcx_dtp -qck -qnodeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/FuncRet3.f
! opt variations: -qnock -qdeferredlp -qreuse=none

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
!*  Function Return - Pointer
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT(K1,K2,K3,K4,K5,N1)    ! (1,2,4,8,16,3)
      INTEGER, KIND             :: K1,K2,K3,K4,K5
      INTEGER, LEN              :: N1

      INTEGER(K1)               :: I1Arr(1)
      INTEGER(K2)               :: I2Arr(1)
      INTEGER(K3)               :: I4Arr(1)
      INTEGER(K4)               :: I8Arr(1)

      REAL(K3)                  :: R4Arr(1)
      REAL(K4)                  :: R8Arr(1)
      REAL(K5)                  :: R16Arr(1)

      COMPLEX(K4)               :: C8Arr(1)
      COMPLEX(K5)               :: C16Arr(1)

      LOGICAL(K1)               :: L1Arr(1)
      LOGICAL(K2)               :: L2Arr(1)
      LOGICAL(K3)               :: L4Arr(1)
      LOGICAL(K4)               :: L8Arr(1)

      CHARACTER(kind=K1,len=N1) :: CharArr(1)

    END TYPE

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT(1,2,4,8,16,*)) :: Arg(:)
  TYPE(DT(1,2,4,8,16,3)), POINTER :: ExtFun(:)
    !ALLOCATE(ExtFun(SIZE(Arg)), SOURCE=Arg) ! not 10.1
    ALLOCATE(ExtFun(SIZE(Arg)))
    ExtFun = Arg
  END FUNCTION

  PROGRAM FuncRet3
  USE M
  IMPLICIT TYPE(DT(1,2,4,8,16,3))(P)

  INTERFACE
    FUNCTION Fun(Arg)
      IMPORT DT
      TYPE(DT(1,2,4,8,16,3)), POINTER :: Fun(:)
      TYPE(DT(1,2,4,8,16,*))          :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun)                  :: ExtFun
  PROCEDURE(Fun),         POINTER :: ProcPtr1
  PROCEDURE(ExtFun),      POINTER :: ProcPtr2
  PROCEDURE(ProcPtr2),    POINTER :: ProcPtr3

  TYPE(DT(1,2,4,8,16,3)), PARAMETER ::  Const=DT(1,2,4,8,16,3)(                  &
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


  TYPE(DT(1,2,4,8,16,3)), PARAMETER :: ConstArr(3)=Const


  CALL IntSub( ExtFun(ConstArr), ConstArr)

  ProcPtr1 => ExtFun
  CALL IntSub( ProcPtr1(ConstArr),ConstArr )

  ProcPtr2 => ProcPtr1
  CALL IntSub( ProcPtr2(ConstArr),ConstArr )

  ProcPtr3 => ProcPtr2
  CALL IntSub( ProcPtr3(ConstArr),ConstArr )

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  TYPE(DT(1,2,4,8,16,*)) :: Arg1(:), Arg2(:)

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

