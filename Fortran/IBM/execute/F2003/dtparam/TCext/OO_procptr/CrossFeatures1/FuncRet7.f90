! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/FuncRet7.f
! opt variations: -qck -qnok -ql -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: FuncRet7.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncRet7.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 27, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 289058 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*   
!*  Function Return - entry stmt 
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT(K1,K2,K3,K4,K5,N1)    ! (1,2,4,8,16,3)
      INTEGER, KIND :: K1,K2,K3,K4,K5
      INTEGER, LEN  :: N1

      INTEGER(K1)   :: I1Arr(1)
      INTEGER(K2)   :: I2Arr(1)
      INTEGER(K3)   :: I4Arr(1)
      INTEGER(K4)   :: I8Arr(1)

      REAL(K3)      :: R4Arr(1)
      REAL(K4)      :: R8Arr(1)
      REAL(K5)      :: R16Arr(1)

      COMPLEX(K4)   :: C8Arr(1)
      COMPLEX(K5)   :: C16Arr(1)

      LOGICAL(K1)   :: L1Arr(1)
      LOGICAL(K2)   :: L2Arr(1)
      LOGICAL(K3)   :: L4Arr(1)
      LOGICAL(K4)   :: L8Arr(1)

      CHARACTER(N1) :: CharArr(1) 
  
    END TYPE

    TYPE :: DT1(K6,K7,K8,K9,K10)    ! (4,1,2,8,16)
      INTEGER, KIND                            :: K6,K7,K8,K9,K10
      TYPE(DT(K7,K8,K6,K9,K10,:)), ALLOCATABLE :: T1
      TYPE(DT(K7,K8,K6,K9,K10,:)), POINTER     :: T2
    END TYPE

    CONTAINS

    FUNCTION ExtFun(Arg)
    TYPE(DT1(4,1,2,8,16)) :: Arg(:)
    TYPE(DT1(4,1,2,8,16)), POINTER :: ExtFun(:)
    TYPE(DT1(4,1,2,8,16)), POINTER :: ExtFun1(:), ExtFun2(:)

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

  END MODULE

  PROGRAM FuncRet6
  USE M
  IMPLICIT TYPE(DT1(4,1,2,8,16))(P) 

  INTERFACE
    FUNCTION Fun(Arg)
      IMPORT DT1
      TYPE(DT1(4,1,2,8,16)), POINTER :: Fun(:)
      TYPE(DT1(4,1,2,8,16))          :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun),        POINTER :: ProcPtr1
  PROCEDURE(ExtFun1),    POINTER :: ProcPtr2
  PROCEDURE(ExtFun2),    POINTER :: ProcPtr3
 
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


  TYPE(DT1(4,1,2,8,16))        :: ConstArr(10000)
  TYPE(DT(1,2,4,8,16,3)), TARGET :: Tar=Const

  ConstArr = DT1(4,1,2,8,16)(Const, Tar)

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
  TYPE(DT1(4,1,2,8,16)) :: Arg1(:), Arg2(:)

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

