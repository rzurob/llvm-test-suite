! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/initExp/Misc/InitExpTypInq3.f
! opt variations: -qck -ql -qreuse=none

!*********************************************************************
!*  ===================================================================
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
!*  Type Parameter Inquiry on derived types  in array constructor
!*
!* (324561)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypInq3
  IMPLICIT NONE

  INTEGER :: I

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

  TYPE(DT(K1=1,K2=2,K4=4,K8=8,K6=16, L128=128)), PARAMETER :: DTP=DT(K1=1,K2=2,K4=4,K8=8,K6=16, L128=128)()

  TYPE :: DTT(K11,K12,K13,K14,K15)    ! (1,2,4,8,16)
    INTEGER, KIND :: K11,K12,K13,K14,K15
    INTEGER(K11)   :: I1(1)=1
    INTEGER(K12)   :: I2(2)=2
    INTEGER(K13)   :: I4(4)=4
    INTEGER(K14)   :: I8(8)=8

    LOGICAL(K11)   :: L1(1)=KIND(DTP%K1) .EQ. 1
    LOGICAL(K12)   :: L2(2)=KIND(DTP%K2) .EQ. 2
    LOGICAL(K13)   :: L4(4)=KIND(DTP%K4) .EQ. 4
    LOGICAL(K14)   :: L8(8)=KIND(DTP%K8) .EQ. 8

    REAL(K13)      :: R4(4)=4
    REAL(K14)      :: R8(8)=8
    REAL(K15)      :: R6(16)=16

    COMPLEX(K13)   :: Z4(4)=(4, -4)
    COMPLEX(K14)   :: Z8(8)=(8, -8)
    COMPLEX(K15)   :: Z6(16)=(16, -16)

    CHARACTER(128) :: CC(1)=CHAR(128)
  END TYPE

  TYPE(DTT(1,2,4,8,16)), PARAMETER ::  T=DTT(1,2,4,8,16)()


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

  IF (ANY( T%I1     .NE. 1))  STOP 61
  IF (ANY( T%I2     .NE. 2))  STOP 62
  IF (ANY( T%I4     .NE. 4))  STOP 63
  IF (ANY( T%I8     .NE. 8))  STOP 64

  IF (ANY( T%L1     .NEQV. .TRUE.))  STOP 61
  IF (ANY( T%L2     .NEQV. .TRUE.))  STOP 62
  IF (ANY( T%L4     .NEQV. .TRUE.))  STOP 63
  IF (ANY( T%L8     .NEQV. .TRUE.))  STOP 64

  IF (ANY( T%R4     .NE. 4 ) ) STOP 71
  IF (ANY( T%R8     .NE. 8 ) ) STOP 72
  IF (ANY( T%R6     .NE. 16) ) STOP 73

  IF (ANY( T%Z4     .NE. (4.,-4.)  )) STOP 81
  IF (ANY( T%Z8     .NE. (8.,-8.)  )) STOP 82
  IF (ANY( T%Z6     .NE. (16.,-16.))) STOP 83

  IF (ANY( T%CC     .NE. CHAR(128) )) STOP 84

  END


