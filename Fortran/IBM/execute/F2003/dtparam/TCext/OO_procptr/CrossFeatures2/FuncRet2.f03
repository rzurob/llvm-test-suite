! GB DTP extension using:
! ftcx_dtp -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_procptr/CrossFeatures2/FuncRet2.f
! opt variations: -qck -qdefaultpv -qnodeferredlp -qreuse=none

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
!*  Function Return - array
!*  (314988)
!*  327080 points out TC problem.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT(K1,K2,K3,K4,K5,N1)    ! (1,2,4,8,16,3)
      INTEGER, KIND            :: K1,K2,K3,K4,K5
      INTEGER, LEN             :: N1

      INTEGER(K1), ALLOCATABLE :: I1Arr(:)
      INTEGER(K2), ALLOCATABLE :: I2Arr(:)
      INTEGER(K3), ALLOCATABLE :: I4Arr(:)
      INTEGER(K4), ALLOCATABLE :: I8Arr(:)

      REAL(K3), ALLOCATABLE     :: R4Arr(:)
      REAL(K4), ALLOCATABLE     :: R8Arr(:)
      REAL(K5), ALLOCATABLE     :: R16Arr(:)

      COMPLEX(K4), ALLOCATABLE :: C8Arr(:)
      COMPLEX(K5), ALLOCATABLE :: C16Arr(:)

      LOGICAL(K1), ALLOCATABLE :: L1Arr(:)
      LOGICAL(K2), ALLOCATABLE :: L2Arr(:)
      LOGICAL(K3), ALLOCATABLE :: L4Arr(:)
      LOGICAL(K4), ALLOCATABLE :: L8Arr(:)

      CHARACTER(N1)            :: CharArr(1)

      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr=>NULL()
      CONTAINS
      PROCEDURE, PASS :: Proc => ModFun

    END TYPE

    TYPE, EXTENDS(DT) :: DT1    ! (1,2,4,8,16,3)
      TYPE(DT(K1,K2,K3,K4,K5,:)), ALLOCATABLE :: T1
      TYPE(DT(K1,K2,K3,K4,K5,:)), POINTER     :: T2
    END TYPE

  CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT(1,2,4,8,16,*)) :: Arg
    CLASS(*), POINTER :: ModFun
      ALLOCATE(ModFun, SOURCE=Arg)
    END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT1(1,2,4,8,16,*)) :: Arg(:)
  !CLASS(*), POINTER :: ExtFun(:)   ! --> too many calls to this lead to huge mem leak.
  CLASS(*), ALLOCATABLE :: ExtFun(:)
    ALLOCATE(ExtFun(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

  PROGRAM FuncRet2
  USE M
  IMPLICIT TYPE(DT1(1,2,4,8,16,3))(P)
  integer, parameter :: arraySize = 10

  INTERFACE
    FUNCTION Fun(Arg)
      IMPORT DT1
      CLASS(*), POINTER :: Fun(:)
      TYPE(DT1(1,2,4,8,16,*))         :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun)                  :: ExtFun
  PROCEDURE(Fun),         POINTER :: ProcPtr1
  PROCEDURE(ExtFun),      POINTER :: ProcPtr2
  PROCEDURE(ProcPtr2),    POINTER :: ProcPtr3

  TYPE(DT1(1,2,4,8,16,3))        :: Arr(arraySize)
  TYPE(DT(1,2,4,8,16,3)), TARGET :: Tar
  TYPE(DT(1,2,4,8,16,3))         ::  Const

  Const = DT(1,2,4,8,16,3)(                                        &
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
  &                            (/"abc"/), NULL()     )


  Const%ProcPtr => ModFun
  Tar = Const
  Arr = DT1(1,2,4,8,16,3)(DT=Const, T1=Const, T2=Tar)

  CALL IntSub( ExtFun(Arr), Arr)

  ProcPtr1 => ExtFun
  CALL IntSub( ProcPtr1(Arr),Arr )

  ProcPtr2 => ProcPtr1
  CALL IntSub( ProcPtr2(Arr),Arr )

  ProcPtr3 => ProcPtr2
  CALL IntSub( ProcPtr3(Arr),Arr )

  DO I=1, 1
    CALL IntSub( (/(Arr(J)%ProcPtr(), J=1,arraySize)/),Arr )
    CALL IntSub( (/(Arr(J)%Proc(), J=1,arraySize)/),Arr )
  END DO

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  CLASS(*)  :: Arg1(:)
  TYPE(DT1(1,2,4,8,16,*)) :: Arg2(:)
  INTEGER   :: I

    DO I=1, SIZE(Arg1)
    SELECT TYPE(Arg1)
    CLASS IS (DT1(1,2,4,8,16,*))

      IF (ANY(Arg1(I)%T1%I1Arr .NE. Arg2(I)%T1%I1Arr)) ERROR STOP 11
      IF (ANY(Arg1(I)%T1%I2Arr .NE. Arg2(I)%T1%I2Arr)) ERROR STOP 12
      IF (ANY(Arg1(I)%T1%I4Arr .NE. Arg2(I)%T1%I4Arr)) ERROR STOP 14
      IF (ANY(Arg1(I)%T1%I8Arr .NE. Arg2(I)%T1%I8Arr)) ERROR STOP 18

      IF (ANY(Arg1(I)%T1%R4Arr  .NE. Arg2(I)%T1%R4Arr))  ERROR STOP 24
      IF (ANY(Arg1(I)%T1%R8Arr  .NE. Arg2(I)%T1%R8Arr))  ERROR STOP 28
      IF (ANY(Arg1(I)%T1%R16Arr .NE. Arg2(I)%T1%R16Arr)) ERROR STOP 26

      IF (ANY(Arg1(I)%T1%C8Arr  .NE. Arg2(I)%T1%C8Arr))  ERROR STOP 38
      IF (ANY(Arg1(I)%T1%C16Arr .NE. Arg2(I)%T1%C16Arr)) ERROR STOP 36

      IF (ANY(Arg1(I)%T1%L1Arr .NEQV. Arg2(I)%T1%L1Arr)) ERROR STOP 41
      IF (ANY(Arg1(I)%T1%L2Arr .NEQV. Arg2(I)%T1%L2Arr)) ERROR STOP 42
      IF (ANY(Arg1(I)%T1%L4Arr .NEQV. Arg2(I)%T1%L4Arr)) ERROR STOP 44
      IF (ANY(Arg1(I)%T1%L8Arr .NEQV. Arg2(I)%T1%L8Arr)) ERROR STOP 48

      IF (ANY(Arg1(I)%T1%CharArr .NE. Arg2(I)%T1%CharArr)) ERROR STOP 68

    CLASS DEFAULT
      STOP 69
    END SELECT
    END DO

    DO I=1, SIZE(Arg1)
    SELECT TYPE(Arg1)
    CLASS IS (DT1(1,2,4,8,16,*))

      IF (ANY(Arg1(I)%T2%I1Arr .NE. Arg2(I)%T2%I1Arr)) ERROR STOP 11
      IF (ANY(Arg1(I)%T2%I2Arr .NE. Arg2(I)%T2%I2Arr)) ERROR STOP 12
      IF (ANY(Arg1(I)%T2%I4Arr .NE. Arg2(I)%T2%I4Arr)) ERROR STOP 14
      IF (ANY(Arg1(I)%T2%I8Arr .NE. Arg2(I)%T2%I8Arr)) ERROR STOP 18

      IF (ANY(Arg1(I)%T2%R4Arr  .NE. Arg2(I)%T2%R4Arr))  ERROR STOP 24
      IF (ANY(Arg1(I)%T2%R8Arr  .NE. Arg2(I)%T2%R8Arr))  ERROR STOP 28
      IF (ANY(Arg1(I)%T2%R16Arr .NE. Arg2(I)%T2%R16Arr)) ERROR STOP 26

      IF (ANY(Arg1(I)%T2%C8Arr  .NE. Arg2(I)%T2%C8Arr))  ERROR STOP 38
      IF (ANY(Arg1(I)%T2%C16Arr .NE. Arg2(I)%T2%C16Arr)) ERROR STOP 36

      IF (ANY(Arg1(I)%T2%L1Arr .NEQV. Arg2(I)%T2%L1Arr)) ERROR STOP 41
      IF (ANY(Arg1(I)%T2%L2Arr .NEQV. Arg2(I)%T2%L2Arr)) ERROR STOP 42
      IF (ANY(Arg1(I)%T2%L4Arr .NEQV. Arg2(I)%T2%L4Arr)) ERROR STOP 44
      IF (ANY(Arg1(I)%T2%L8Arr .NEQV. Arg2(I)%T2%L8Arr)) ERROR STOP 48

      IF (ANY(Arg1(I)%T2%CharArr .NE. Arg2(I)%T2%CharArr)) ERROR STOP 68

    CLASS DEFAULT
      STOP 69
    END SELECT
    END DO

  END SUBROUTINE

  END

