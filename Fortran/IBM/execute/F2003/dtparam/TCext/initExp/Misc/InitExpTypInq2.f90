! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/initExp/Misc/InitExpTypInq2.f
! opt variations: -qck -qnol -qreuse=none

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

  type params
    integer(1) :: k1
    integer(2) :: k2
    integer(4) :: k4
    integer(8) :: k8
    integer(8) :: k6
    integer    :: l128
  end type

!  TYPE(DT(K1=1,K2=2,K4=4,K8=8,K6=16, L128=128)), PARAMETER :: DTP=DT(1,2,4,8,16,128)()
!  TYPE(DT(K1=1,K2=2,K4=4,K8=8,K6=16, L128=128)), PARAMETER :: DTPC=DT(K1=1,K2=2,K4=4,K8=8,K6=16, L128=128)()
  TYPE(params), PARAMETER :: DTP=params(1,2,4,8,16,128)
  TYPE(params), PARAMETER :: DTPC=params(K1=1,K2=2,K4=4,K8=8,K6=16, L128=128)

  TYPE :: DTT(N1,X1,X2,X3,X4,X5)    ! (20,1,2,4,8,16)
    INTEGER, KIND :: X1,X2,X3,X4,X5
    INTEGER, LEN  :: N1
    INTEGER(X1)   :: I1(DTP%K1) = DTP%K1
    INTEGER(X2)   :: I2(DTP%K2) = DTP%K2
    INTEGER(X3)   :: I4(DTP%K4) = DTP%K4
    INTEGER(X4)   :: I8(DTP%K8) = DTP%K8

    LOGICAL(X1)   :: L1(DTP%K1) = .FALSE.
    LOGICAL(X2)   :: L2(DTP%K2) = .FALSE.
    LOGICAL(X3)   :: L4(DTP%K4) = .FALSE.
    LOGICAL(X4)   :: L8(DTP%K8) = .FALSE.

    REAL(X3)      :: R4(DTP%K4) = DTP%K4
    REAL(X4)      :: R8(DTP%K8) = DTP%K8
    REAL(X5)      :: R6(DTP%K6) = DTP%K6

    COMPLEX(X3)   :: Z4(DTP%K4) = (0,0)
    COMPLEX(X4)   :: Z8(DTP%K8) = (0,0)
    COMPLEX(X5)   :: Z6(DTP%K6) = (0,0)

    CHARACTER(DTPC%L128) :: CC(DTP%K1)=['']
  END TYPE

  TYPE(DTT(20,1,2,4,8,16)), PARAMETER ::  T=DTT(20,1,2,4,8,16)()


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


