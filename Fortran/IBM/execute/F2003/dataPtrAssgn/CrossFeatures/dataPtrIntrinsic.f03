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

  TYPE :: DT
    INTEGER(1),   POINTER  :: PtrI1(:,:) =>NULL()
    INTEGER(2),   POINTER  :: PtrI2(:,:) =>NULL()
    INTEGER(4),   POINTER  :: PtrI4(:,:) =>NULL()
    INTEGER(8),   POINTER  :: PtrI8(:,:) =>NULL()

    REAL(4),      POINTER  :: PtrR4(:,:) =>NULL()
    REAL(8),      POINTER  :: PtrR8(:,:) =>NULL()
    REAL(16),     POINTER  :: PtrR16(:,:) =>NULL()

    COMPLEX(4),   POINTER  :: PtrC4(:,:) =>NULL()
    COMPLEX(8),   POINTER  :: PtrC8(:,:) =>NULL()
    COMPLEX(16),  POINTER  :: PtrC16(:,:)=>NULL()

    LOGICAL(1),   POINTER  :: PtrL1(:,:) =>NULL()
    LOGICAL(2),   POINTER  :: PtrL2(:,:) =>NULL()
    LOGICAL(4),   POINTER  :: PtrL4(:,:) =>NULL()
    LOGICAL(8),   POINTER  :: PtrL8(:,:) =>NULL()

    CHARACTER(1), POINTER  :: PtrC1(:,:) =>NULL()
  END TYPE

  TYPE :: DT1
    INTEGER(1)   :: PtrI1Tar(100, 100)=-1_1
    INTEGER(2)   :: PtrI2Tar(100, 100)=-2_2
    INTEGER(4)   :: PtrI4Tar(100, 100)=-4_4
    INTEGER(8)   :: PtrI8Tar(100, 100)=-8_8

    REAL(4)      :: PtrR4Tar(100, 100)=-4.0_4
    REAL(8)      :: PtrR8Tar(100, 100)=-8.0_8
    REAL(16)     :: PtrR16Tar(100, 100)=-16.0_16

    COMPLEX(4)   :: PtrC4Tar(100, 100)=(4.0_4, -4.0_4)
    COMPLEX(8)   :: PtrC8Tar(100, 100)=(8.0_8, -8.0_8)
    COMPLEX(16)  :: PtrC16Tar(100, 100)=(16.0_16, -16.0_16)

    LOGICAL(1)   :: PtrL1Tar(100, 100)=.TRUE._1
    LOGICAL(2)   :: PtrL2Tar(100, 100)=.TRUE._2
    LOGICAL(4)   :: PtrL4Tar(100, 100)=.TRUE._4
    LOGICAL(8)   :: PtrL8Tar(100, 100)=.TRUE._8

    CHARACTER(1) :: PtrC1Tar(100, 100)= "1"
  END TYPE

  TYPE (DT)          :: P
  TYPE (DT1), TARGET :: T

  P%PtrI1(10:19, 1:9 ) => T%PtrI1Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrI1))                    ERROR STOP 11
  IF (ANY( LBOUND(P%PtrI1) .NE. (/10, 1 /)))        ERROR STOP 12
  IF (ANY( UBOUND(P%PtrI1) .NE. (/19, 9 /)))        ERROR STOP 13
  IF (ANY( P%PtrI1         .NE. -1_1))              ERROR STOP 14

  P%PtrI2(10:19, 1:9 ) => T%PtrI2Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrI2))                    ERROR STOP 21
  IF (ANY( LBOUND(P%PtrI2) .NE. (/10, 1 /)))        ERROR STOP 22
  IF (ANY( UBOUND(P%PtrI2) .NE. (/19, 9 /)))        ERROR STOP 23
  IF (ANY( P%PtrI2         .NE. -2_2))              ERROR STOP 24

  P%PtrI4(10:19, 1:9 ) => T%PtrI4Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrI4))                    ERROR STOP 31
  IF (ANY( LBOUND(P%PtrI4) .NE. (/10, 1 /)))        ERROR STOP 32
  IF (ANY( UBOUND(P%PtrI4) .NE. (/19, 9 /)))        ERROR STOP 33
  IF (ANY( P%PtrI4         .NE. -4_4))              ERROR STOP 34

  P%PtrI8(10:19, 1:9 ) => T%PtrI8Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrI8))                    ERROR STOP 41
  IF (ANY( LBOUND(P%PtrI8) .NE. (/10, 1 /)))        ERROR STOP 42
  IF (ANY( UBOUND(P%PtrI8) .NE. (/19, 9 /)))        ERROR STOP 43
  IF (ANY( P%PtrI8         .NE. -8_8))              ERROR STOP 44


  P%PtrR4(10:19, 1:9 ) => T%PtrR4Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrR4))                    ERROR STOP 51
  IF (ANY( LBOUND(P%PtrR4) .NE. (/10, 1 /)))        ERROR STOP 52
  IF (ANY( UBOUND(P%PtrR4) .NE. (/19, 9 /)))        ERROR STOP 53
  IF (ANY( P%PtrR4         .NE. -4.0_4))            ERROR STOP 54

  P%PtrR8(10:19, 1:9 ) => T%PtrR8Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrR8))                    ERROR STOP 61
  IF (ANY( LBOUND(P%PtrR8) .NE. (/10, 1 /)))        ERROR STOP 62
  IF (ANY( UBOUND(P%PtrR8) .NE. (/19, 9 /)))        ERROR STOP 63
  IF (ANY( P%PtrR8         .NE. -8.0_8))            ERROR STOP 64

  P%PtrR16(10:19, 1:9 ) => T%PtrR16Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrR16))                   ERROR STOP 71
  IF (ANY( LBOUND(P%PtrR16) .NE. (/10, 1 /)))       ERROR STOP 72
  IF (ANY( UBOUND(P%PtrR16) .NE. (/19, 9 /)))       ERROR STOP 73
  IF (ANY( P%PtrR16         .NE. -16.0_16))         ERROR STOP 74


  P%PtrC4(10:19, 1:9 ) => T%PtrC4Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrC4))                    ERROR STOP 81
  IF (ANY( LBOUND(P%PtrC4) .NE. (/10, 1 /)))        ERROR STOP 82
  IF (ANY( UBOUND(P%PtrC4) .NE. (/19, 9 /)))        ERROR STOP 83
  IF (ANY( P%PtrC4         .NE. (4.0_4, -4.0_4)))   ERROR STOP 84

  P%PtrC8(10:19, 1:9 ) => T%PtrC8Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrR8))                    ERROR STOP 91
  IF (ANY( LBOUND(P%PtrC8) .NE. (/10, 1 /)))        ERROR STOP 92
  IF (ANY( UBOUND(P%PtrC8) .NE. (/19, 9 /)))        ERROR STOP 93
  IF (ANY( P%PtrC8         .NE. (8.0_8, -8.0_8)))   ERROR STOP 94

  P%PtrC16(10:19, 1:9 ) => T%PtrC16Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrC16))                   ERROR STOP 15
  IF (ANY( LBOUND(P%PtrC16) .NE. (/10, 1 /)))       ERROR STOP 16
  IF (ANY( UBOUND(P%PtrC16) .NE. (/19, 9 /)))       ERROR STOP 17
  IF (ANY( P%PtrC16         .NE. (16.0_16, -16.0_16 )))    ERROR STOP 18


  P%PtrL1(10:19, 1:9 ) => T%PtrL1Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL1))                    ERROR STOP 25
  IF (ANY( LBOUND(P%PtrL1) .NE. (/10, 1 /)))        ERROR STOP 26
  IF (ANY( UBOUND(P%PtrL1) .NE. (/19, 9 /)))        ERROR STOP 27
  IF (ANY( P%PtrL1         .NEQV. .TRUE._1))        ERROR STOP 28

  P%PtrL2(10:19, 1:9 ) => T%PtrL2Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL2))                    ERROR STOP 35
  IF (ANY( LBOUND(P%PtrL2) .NE. (/10, 1 /)))        ERROR STOP 36
  IF (ANY( UBOUND(P%PtrL2) .NE. (/19, 9 /)))        ERROR STOP 37
  IF (ANY( P%PtrL2         .NEQV. .TRUE._2))        ERROR STOP 38

  P%PtrL4(10:19, 1:9 ) => T%PtrL4Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL4))                    ERROR STOP 45
  IF (ANY( LBOUND(P%PtrL4) .NE. (/10, 1 /)))        ERROR STOP 46
  IF (ANY( UBOUND(P%PtrL4) .NE. (/19, 9 /)))        ERROR STOP 47
  IF (ANY( P%PtrL4         .NEQV. .TRUE._4))        ERROR STOP 48

  P%PtrL8(10:19, 1:9 ) => T%PtrL8Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL8))                    ERROR STOP 55
  IF (ANY( LBOUND(P%PtrL8) .NE. (/10, 1 /)))        ERROR STOP 56
  IF (ANY( UBOUND(P%PtrL8) .NE. (/19, 9 /)))        ERROR STOP 57
  IF (ANY( P%PtrL8         .NEQV. .TRUE._8))        ERROR STOP 58

  P%PtrC1(10:19, 1:9 ) => T%PtrC1Tar(:, 10)
  IF (.NOT. ASSOCIATED(P%PtrL1))                    ERROR STOP 65
  IF (ANY( LBOUND(P%PtrC1) .NE. (/10, 1 /)))        ERROR STOP 66
  IF (ANY( UBOUND(P%PtrC1) .NE. (/19, 9 /)))        ERROR STOP 67
  IF (ANY( P%PtrC1         .NE. "1" ))              ERROR STOP 68




  END

