! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/FuncRet1.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnock -qreuse=self

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
!*  FuncTion Return - Derived types
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT(K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14,N1)    ! (1,2,4,8,4,8,16,8,16,1,2,4,8,1,3)
      INTEGER, KIND              :: K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14
      INTEGER, LEN               :: N1

      INTEGER(K1)                :: I1Arr(1)
      INTEGER(K2)                :: I2Arr(1)
      INTEGER(K3)                :: I4Arr(1)
      INTEGER(K4)                :: I8Arr(1)

      REAL(K5)                   :: R4Arr(1)
      REAL(K6)                   :: R8Arr(1)
      REAL(K7)                   :: R16Arr(1)

      COMPLEX(K8)                :: C8Arr(1)
      COMPLEX(K9)                :: C16Arr(1)

      LOGICAL(K10)               :: L1Arr(1)
      LOGICAL(K11)               :: L2Arr(1)
      LOGICAL(K12)               :: L4Arr(1)
      LOGICAL(K13)               :: L8Arr(1)

      CHARACTER(kind=K14,len=N1) :: CharArr(1)

    END TYPE

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT(1,2,4,8,4,8,16,8,16,1,2,4,8,1,3)) :: ExtFun, Arg
    ExtFun = Arg
  END FUNCTION

  PROGRAM FuncRet1
  USE M
  IMPLICIT TYPE(DT(1,2,4,8,4,8,16,8,16,1,2,4,8,1,3))(P)

  INTERFACE
    FUNCTION Fun(Arg)
      IMPORT DT
      TYPE(DT(1,2,4,8,4,8,16,8,16,1,2,4,8,1,3)) :: Fun, Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun)          :: ExtFun
  PROCEDURE(Fun), POINTER :: ProcPtr1
  PROCEDURE(Fun), POINTER :: ProcPtr2
  PROCEDURE(Fun), POINTER :: ProcPtr3

  TYPE(DT(1,2,4,8,4,8,16,8,16,1,2,4,8,1,3)), PARAMETER ::  Const=DT(1,2,4,8,4,8,16,8,16,1,2,4,8,1,3)(                  &
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

  ProcPtr1 => ExtFun
  CALL IntSub( ProcPtr1(Const), Const)

  ProcPtr2 => ExtFun
  CALL IntSub( ProcPtr2(Const), Const)

  ProcPtr3 => ExtFun
  CALL IntSub( ProcPtr3(Const), Const)

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  TYPE(DT(1,2,4,8,4,8,16,8,16,1,2,4,8,1,*)) :: Arg1, Arg2
    IF (ANY(Arg1%I1Arr .NE. Arg2%I1Arr)) ERROR STOP 11
    IF (ANY(Arg1%I2Arr .NE. Arg2%I2Arr)) ERROR STOP 12
    IF (ANY(Arg1%I4Arr .NE. Arg2%I4Arr)) ERROR STOP 14
    IF (ANY(Arg1%I8Arr .NE. Arg2%I8Arr)) ERROR STOP 18

    IF (ANY(Arg1%R4Arr  .NE. Arg2%R4Arr))  ERROR STOP 24
    IF (ANY(Arg1%R8Arr  .NE. Arg2%R8Arr))  ERROR STOP 28
    IF (ANY(Arg1%R16Arr .NE. Arg2%R16Arr)) ERROR STOP 216

    IF (ANY(Arg1%C8Arr  .NE. Arg2%C8Arr))  ERROR STOP 38
    IF (ANY(Arg1%C16Arr .NE. Arg2%C16Arr)) ERROR STOP 316

    IF (ANY(Arg1%L1Arr .NEQV. Arg2%L1Arr)) ERROR STOP 41
    IF (ANY(Arg1%L2Arr .NEQV. Arg2%L2Arr)) ERROR STOP 42
    IF (ANY(Arg1%L4Arr .NEQV. Arg2%L4Arr)) ERROR STOP 44
    IF (ANY(Arg1%L8Arr .NEQV. Arg2%L8Arr)) ERROR STOP 48

    IF (ANY(Arg1%CharArr .NE. Arg2%CharArr)) ERROR STOP 68

  END SUBROUTINE

  END
