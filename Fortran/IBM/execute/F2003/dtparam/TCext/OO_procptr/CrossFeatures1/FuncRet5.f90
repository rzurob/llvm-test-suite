! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=none /tstdev/OO_procptr/CrossFeatures1/FuncRet5.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=self

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: FuncRet5.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncRet5.f
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
!*  Function Return -Proc ptr
!*  (304320)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT(K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,N1)    ! (1,2,4,8,4,8,16,8,16,1,2,4,8,3)
      INTEGER, KIND :: K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13
      INTEGER, LEN  :: N1

      INTEGER(K1)   :: I1Arr(1)
      INTEGER(K2)   :: I2Arr(1)
      INTEGER(K3)   :: I4Arr(1)
      INTEGER(K4)   :: I8Arr(1)

      REAL(K5)      :: R4Arr(1)
      REAL(K6)      :: R8Arr(1)
      REAL(K7)      :: R16Arr(1)

      COMPLEX(K8)   :: C8Arr(1)
      COMPLEX(K9)   :: C16Arr(1)

      LOGICAL(K10)  :: L1Arr(1)
      LOGICAL(K11)  :: L2Arr(1)
      LOGICAL(K12)  :: L4Arr(1)
      LOGICAL(K13)  :: L8Arr(1)

      CHARACTER(N1) :: CharArr(1)

    END TYPE

    TYPE :: DT1(K14,N2,K15,K16,K17,K18,N3)    ! (4,20,1,2,8,16,3)
      INTEGER, KIND                                                                 :: K14,K15,K16,K17,K18
      INTEGER, LEN                                                                  :: N2,N3
      TYPE(DT(K15,K16,K14,K17,K14,K17,K18,K17,K18,K15,K16,K14,K17,N3)), ALLOCATABLE :: T1
      TYPE(DT(K15,K16,K14,K17,K14,K17,K18,K17,K18,K15,K16,K14,K17,N3)), POINTER     :: T2
    END TYPE

    INTERFACE
      FUNCTION Fun(Arg)
        IMPORT DT1
        TYPE(DT1(4,20,1,2,8,16,3)), POINTER :: Fun(:)
        TYPE(DT1(4,*,1,2,8,16,*))           :: Arg(:)
      END FUNCTION
    END INTERFACE

    CONTAINS

    FUNCTION ModFun(Arg)
    PROCEDURE(Fun) :: Arg
    PROCEDURE(Fun), POINTER :: ModFun
      ModFun => Arg
    END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT1(4,*,1,2,8,16,*)) :: Arg(:)
  TYPE(DT1(4,20,1,2,8,16,3)), POINTER :: ExtFun(:)
    !ALLOCATE(ExtFun(SIZE(Arg)), SOURCE=Arg)  ! not 10.1
    ALLOCATE(ExtFun(SIZE(Arg)))
    ExtFun = Arg
  END FUNCTION

  PROGRAM FuncRet5
  USE M
  IMPLICIT TYPE(DT1(4,20,1,2,8,16,3))(P)

  PROCEDURE(Fun)                  :: ExtFun
  PROCEDURE(Fun),         POINTER :: ProcPtr1
  PROCEDURE(ExtFun),      POINTER :: ProcPtr2
  PROCEDURE(ProcPtr2),    POINTER :: ProcPtr3

  TYPE(DT(1,2,4,8,4,8,16,8,16,1,2,4,8,3)), PARAMETER ::  Const=DT(1,2,4,8,4,8,16,8,16,1,2,4,8,3)(                  &
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


  TYPE(DT(1,2,4,8,4,8,16,8,16,1,2,4,8,3)), TARGET :: Tar=Const
  TYPE(DT1(4,20,1,2,8,16,3))        :: ConstArr(30000)

  ConstArr = DT1(4,20,1,2,8,16,3)(Const, Tar)

  CALL IntSub( ExtFun(ConstArr), ConstArr)

  ProcPtr1 => ModFun(ExtFun)
  CALL IntSub( ProcPtr1(ConstArr),ConstArr )

  ProcPtr2 => ModFun(ExtFun)
  CALL IntSub( ProcPtr2(ConstArr),ConstArr )

  ProcPtr3 => ModFun(ExtFun)
  CALL IntSub( ProcPtr3(ConstArr),ConstArr )

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  TYPE(DT1(4,*,1,2,8,16,*)) :: Arg1(:), Arg2(:)

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

