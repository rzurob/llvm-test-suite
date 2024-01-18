! GB DTP extension using:
! ftcx_dtp -qreuse=none /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrIntrinsic.f
! opt variations: -qck -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 07, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  intrinsic types
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrIntrinsic
  IMPLICIT NONE

  TYPE :: DT(K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14,N1)    ! (1,2,4,8,4,8,16,4,8,16,1,2,4,8,1)
    INTEGER, KIND           :: K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14
    INTEGER, LEN            :: N1
    INTEGER(K1), POINTER     :: PtrI1(:,:) =>NULL()
    INTEGER(K2), POINTER     :: PtrI2(:,:) =>NULL()
    INTEGER(K3), POINTER     :: PtrI4(:,:) =>NULL()
    INTEGER(K4), POINTER     :: PtrI8(:,:) =>NULL()

    REAL(K5), POINTER        :: PtrR4(:,:) =>NULL()
    REAL(K6), POINTER        :: PtrR8(:,:) =>NULL()
    REAL(K7), POINTER        :: PtrR16(:,:) =>NULL()

    COMPLEX(K8), POINTER     :: PtrC4(:,:) =>NULL()
    COMPLEX(K9), POINTER     :: PtrC8(:,:) =>NULL()
    COMPLEX(K10), POINTER    :: PtrC16(:,:)=>NULL()

    LOGICAL(K11), POINTER    :: PtrL1(:,:) =>NULL()
    LOGICAL(K12), POINTER    :: PtrL2(:,:) =>NULL()
    LOGICAL(K13), POINTER    :: PtrL4(:,:) =>NULL()
    LOGICAL(K14), POINTER    :: PtrL8(:,:) =>NULL()

    CHARACTER(N1), POINTER   :: PtrC1(:,:) =>NULL()
  END TYPE

  TYPE :: DT1(K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28,N2)    ! (1,2,4,8,4,8,16,4,8,16,1,2,4,8,1)
    INTEGER, KIND :: K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28
    INTEGER, LEN  :: N2
    INTEGER(K15)  :: PtrI1Tar(100, 100)=-1_1
    INTEGER(K16)  :: PtrI2Tar(100, 100)=-2_2
    INTEGER(K17)  :: PtrI4Tar(100, 100)=-4_4
    INTEGER(K18)  :: PtrI8Tar(100, 100)=-8_8

    REAL(K19)     :: PtrR4Tar(100, 100)=-4.0_4
    REAL(K20)     :: PtrR8Tar(100, 100)=-8.0_8
    REAL(K21)     :: PtrR16Tar(100, 100)=-16.0_16

    COMPLEX(K22)  :: PtrC4Tar(100, 100)=(4.0_4, -4.0_4)
    COMPLEX(K23)  :: PtrC8Tar(100, 100)=(8.0_8, -8.0_8)
    COMPLEX(K24)  :: PtrC16Tar(100, 100)=(16.0_16, -16.0_16)

    LOGICAL(K25)  :: PtrL1Tar(100, 100)=.TRUE._1
    LOGICAL(K26)  :: PtrL2Tar(100, 100)=.TRUE._2
    LOGICAL(K27)  :: PtrL4Tar(100, 100)=.TRUE._4
    LOGICAL(K28)  :: PtrL8Tar(100, 100)=.TRUE._8

    CHARACTER(N2) :: PtrC1Tar(100, 100)= "1"
  END TYPE

  TYPE (DT(1,2,4,8,4,8,16,4,8,16,1,2,4,8,1))          :: P
  TYPE (DT1(1,2,4,8,4,8,16,4,8,16,1,2,4,8,1)), TARGET :: T

  P%PtrI1(10:19, 1:9 ) => T%PtrI1Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrI1))                    STOP 11
  IF (ANY( LBOUND(P%PtrI1) .NE. (/10, 1 /)))        STOP 12
  IF (ANY( UBOUND(P%PtrI1) .NE. (/19, 9 /)))        STOP 13
  IF (ANY( P%PtrI1         .NE. -1_1))              STOP 14

  P%PtrI2(10:19, 1:9 ) => T%PtrI2Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrI2))                    STOP 21
  IF (ANY( LBOUND(P%PtrI2) .NE. (/10, 1 /)))        STOP 22
  IF (ANY( UBOUND(P%PtrI2) .NE. (/19, 9 /)))        STOP 23
  IF (ANY( P%PtrI2         .NE. -2_2))              STOP 24

  P%PtrI4(10:19, 1:9 ) => T%PtrI4Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrI4))                    STOP 31
  IF (ANY( LBOUND(P%PtrI4) .NE. (/10, 1 /)))        STOP 32
  IF (ANY( UBOUND(P%PtrI4) .NE. (/19, 9 /)))        STOP 33
  IF (ANY( P%PtrI4         .NE. -4_4))              STOP 34

  P%PtrI8(10:19, 1:9 ) => T%PtrI8Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrI8))                    STOP 41
  IF (ANY( LBOUND(P%PtrI8) .NE. (/10, 1 /)))        STOP 42
  IF (ANY( UBOUND(P%PtrI8) .NE. (/19, 9 /)))        STOP 43
  IF (ANY( P%PtrI8         .NE. -8_8))              STOP 44


  P%PtrR4(10:19, 1:9 ) => T%PtrR4Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrR4))                    STOP 51
  IF (ANY( LBOUND(P%PtrR4) .NE. (/10, 1 /)))        STOP 52
  IF (ANY( UBOUND(P%PtrR4) .NE. (/19, 9 /)))        STOP 53
  IF (ANY( P%PtrR4         .NE. -4.0_4))            STOP 54

  P%PtrR8(10:19, 1:9 ) => T%PtrR8Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrR8))                    STOP 61
  IF (ANY( LBOUND(P%PtrR8) .NE. (/10, 1 /)))        STOP 62
  IF (ANY( UBOUND(P%PtrR8) .NE. (/19, 9 /)))        STOP 63
  IF (ANY( P%PtrR8         .NE. -8.0_8))            STOP 64

  P%PtrR16(10:19, 1:9 ) => T%PtrR16Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrR16))                   STOP 71
  IF (ANY( LBOUND(P%PtrR16) .NE. (/10, 1 /)))       STOP 72
  IF (ANY( UBOUND(P%PtrR16) .NE. (/19, 9 /)))       STOP 73
  IF (ANY( P%PtrR16         .NE. -16.0_16))         STOP 74


  P%PtrC4(10:19, 1:9 ) => T%PtrC4Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrC4))                    STOP 81
  IF (ANY( LBOUND(P%PtrC4) .NE. (/10, 1 /)))        STOP 82
  IF (ANY( UBOUND(P%PtrC4) .NE. (/19, 9 /)))        STOP 83
  IF (ANY( P%PtrC4         .NE. (4.0_4, -4.0_4)))   STOP 84

  P%PtrC8(10:19, 1:9 ) => T%PtrC8Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrR8))                    STOP 91
  IF (ANY( LBOUND(P%PtrC8) .NE. (/10, 1 /)))        STOP 92
  IF (ANY( UBOUND(P%PtrC8) .NE. (/19, 9 /)))        STOP 93
  IF (ANY( P%PtrC8         .NE. (8.0_8, -8.0_8)))   STOP 94

  P%PtrC16(10:19, 1:9 ) => T%PtrC16Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrC16))                   STOP 15
  IF (ANY( LBOUND(P%PtrC16) .NE. (/10, 1 /)))       STOP 16
  IF (ANY( UBOUND(P%PtrC16) .NE. (/19, 9 /)))       STOP 17
  IF (ANY( P%PtrC16         .NE. (16.0_16, -16.0_16 )))    STOP 18


  P%PtrL1(10:19, 1:9 ) => T%PtrL1Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL1))                    STOP 25
  IF (ANY( LBOUND(P%PtrL1) .NE. (/10, 1 /)))        STOP 26
  IF (ANY( UBOUND(P%PtrL1) .NE. (/19, 9 /)))        STOP 27
  IF (ANY( P%PtrL1         .NEQV. .TRUE._1))        STOP 28

  P%PtrL2(10:19, 1:9 ) => T%PtrL2Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL2))                    STOP 35
  IF (ANY( LBOUND(P%PtrL2) .NE. (/10, 1 /)))        STOP 36
  IF (ANY( UBOUND(P%PtrL2) .NE. (/19, 9 /)))        STOP 37
  IF (ANY( P%PtrL2         .NEQV. .TRUE._2))        STOP 38

  P%PtrL4(10:19, 1:9 ) => T%PtrL4Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL4))                    STOP 45
  IF (ANY( LBOUND(P%PtrL4) .NE. (/10, 1 /)))        STOP 46
  IF (ANY( UBOUND(P%PtrL4) .NE. (/19, 9 /)))        STOP 47
  IF (ANY( P%PtrL4         .NEQV. .TRUE._4))        STOP 48

  P%PtrL8(10:19, 1:9 ) => T%PtrL8Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL8))                    STOP 55
  IF (ANY( LBOUND(P%PtrL8) .NE. (/10, 1 /)))        STOP 56
  IF (ANY( UBOUND(P%PtrL8) .NE. (/19, 9 /)))        STOP 57
  IF (ANY( P%PtrL8         .NEQV. .TRUE._8))        STOP 58

  P%PtrC1(10:19, 1:9 ) => T%PtrC1Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL1))                    STOP 65
  IF (ANY( LBOUND(P%PtrC1) .NE. (/10, 1 /)))        STOP 66
  IF (ANY( UBOUND(P%PtrC1) .NE. (/19, 9 /)))        STOP 67
  IF (ANY( P%PtrC1         .NE. "1" ))              STOP 68




  END


