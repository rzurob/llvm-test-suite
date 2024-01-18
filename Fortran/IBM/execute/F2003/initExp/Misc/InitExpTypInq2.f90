!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypInq2.f
!*
!*  DATE                       : Aug. 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Type Parameter Inquiry on derived types
!*
!* (324561)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypInq2
  IMPLICIT NONE

  TYPE :: DT0(K1, K2, K4, K8, K6)
    INTEGER(1), KIND :: K1=0
    INTEGER(2), KIND :: K2=0
    INTEGER(4), KIND :: K4=0
    INTEGER(8), KIND :: K8=0
    INTEGER(8), KIND :: K6=0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(L128)
    INTEGER, LEN ::L128
  END TYPE

  TYPE(DT(K1=1,K2=2,K4=4,K8=8,K6=16, L128=128)) :: DTP

  TYPE :: DTT
    INTEGER(1) :: I1(DTP%K1) = DTP%K1
    INTEGER(2) :: I2(DTP%K2) = DTP%K2
    INTEGER(4) :: I4(DTP%K4) = DTP%K4
    INTEGER(8) :: I8(DTP%K8) = DTP%K8

    LOGICAL(1) :: L1(DTP%K1) = .FALSE.
    LOGICAL(2) :: L2(DTP%K2) = .FALSE.
    LOGICAL(4) :: L4(DTP%K4) = .FALSE.
    LOGICAL(8) :: L8(DTP%K8) = .FALSE.

    REAL(4)    :: R4(DTP%K4) = DTP%K4
    REAL(8)    :: R8(DTP%K8) = DTP%K8
    REAL(16)   :: R6(DTP%K6) = DTP%K6

    COMPLEX(4) :: Z4(DTP%K4) = (0,0)
    COMPLEX(8) :: Z8(DTP%K8) = (0,0)
    COMPLEX(16):: Z6(DTP%K6) = (0,0)

    CHARACTER(DTP%L128) :: CC(DTP%K1)=['']
  END TYPE

  TYPE(DTT), PARAMETER ::  T=DTT()


  IF (SIZE(T%I1)    .NE. 1  ) STOP 11
  IF (SIZE(T%I2)    .NE. 2  ) STOP 12
  IF (SIZE(T%I4)    .NE. 4  ) STOP 14
  IF (SIZE(T%I8)    .NE. 8  ) STOP 18

  IF (SIZE(T%L1)    .NE. 1  ) STOP 21
  IF (SIZE(T%L2)    .NE. 2  ) STOP 22
  IF (SIZE(T%L4)    .NE. 4  ) STOP 24
  IF (SIZE(T%L8)    .NE. 8  ) STOP 28

  IF (SIZE(T%R4)    .NE. 4  ) STOP 31
  IF (SIZE(T%R8)    .NE. 8  ) STOP 32
  IF (SIZE(T%R6)    .NE. 16 ) STOP 33

  IF (SIZE(T%Z4)    .NE. 4  ) STOP 41
  IF (SIZE(T%Z8)    .NE. 8  ) STOP 42
  IF (SIZE(T%Z6)    .NE. 16 ) STOP 43

  IF (SIZE(T%CC)    .NE. 1  ) STOP 51
  IF (LEN(T%CC)     .NE. 128) STOP 52


  END


