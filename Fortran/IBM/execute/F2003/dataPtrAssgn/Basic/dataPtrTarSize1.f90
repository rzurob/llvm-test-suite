!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 06, 2006
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
!*  The elements of the target of data-pointer-object, in array element order
!*  (6.2.2.2), are the first SIZE(data-pointer-object) elements of data-target.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrTarSize1
  IMPLICIT NONE

  INTEGER              :: I

  INTEGER(1), POINTER  :: PtrI1(:,:) =>NULL()
  INTEGER(1), TARGET   :: PtrI1Tar(100, 10)=RESHAPE((/(-I, I=1, 1000)/), (/100,10/))

  INTEGER(2), POINTER  :: PtrI2(:,:) =>NULL()
  INTEGER(2), TARGET   :: PtrI2Tar(100, 10)=RESHAPE((/(-I, I=1, 1000)/), (/100,10/))

  INTEGER(4), POINTER  :: PtrI4(:,:) =>NULL()
  INTEGER(4), TARGET   :: PtrI4Tar(100, 10)=RESHAPE((/(-I, I=1, 1000)/), (/100,10/))

  INTEGER(8), POINTER  :: PtrI8(:,:) =>NULL()
  INTEGER(8), TARGET   :: PtrI8Tar(100, 10)=RESHAPE((/(-I, I=1, 1000)/), (/100,10/))

  REAL(4), POINTER     :: PtrR4(:,:) =>NULL()
  REAL(4), TARGET      :: PtrR4Tar(100, 10)=RESHAPE((/(-I, I=1, 1000)/), (/100,10/))

  REAL(8), POINTER     :: PtrR8(:,:) =>NULL()
  REAL(8), TARGET      :: PtrR8Tar(100, 10)=RESHAPE((/(-I, I=1, 1000)/), (/100,10/))

  REAL(16), POINTER    :: PtrR16(:,:) =>NULL()
  REAL(16), TARGET     :: PtrR16Tar(100, 10)=RESHAPE((/(-I, I=1, 1000)/), (/100,10/))

  COMPLEX(4), POINTER  :: PtrC4(:,:) =>NULL()
  COMPLEX(4), TARGET   :: PtrC4Tar(100, 10)=RESHAPE((/((I,-I), I=1, 1000)/), (/100,10/))

  COMPLEX(8), POINTER  :: PtrC8(:,:) =>NULL()
  COMPLEX(8), TARGET   :: PtrC8Tar(100, 10)=RESHAPE((/((I,-I), I=1, 1000)/), (/100,10/))

  COMPLEX(16), POINTER :: PtrC16(:,:) =>NULL()
  COMPLEX(16), TARGET  :: PtrC16Tar(100, 10)=RESHAPE((/((I,-I), I=1, 1000)/), (/100,10/))

  LOGICAL(1), POINTER  :: PtrL1(:,:) =>NULL()
  LOGICAL(1), TARGET   :: PtrL1Tar(100, 10)=RESHAPE((/(.TRUE., I=1, 1000)/), (/100,10/))

  LOGICAL(2), POINTER  :: PtrL2(:,:) =>NULL()
  LOGICAL(2), TARGET   :: PtrL2Tar(100, 10)=RESHAPE((/(.TRUE., I=1, 1000)/), (/100,10/))

  LOGICAL(4), POINTER  :: PtrL4(:,:) =>NULL()
  LOGICAL(4), TARGET   :: PtrL4Tar(100, 10)=RESHAPE((/(.TRUE., I=1, 1000)/), (/100,10/))

  LOGICAL(8), POINTER  :: PtrL8(:,:) =>NULL()
  LOGICAL(8), TARGET   :: PtrL8Tar(100, 10)=RESHAPE((/(.TRUE., I=1, 1000)/), (/100,10/))


  PtrI1(10:19, 1:9 ) => PtrI1Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrI1))                    STOP 11
  IF (ANY( LBOUND(PtrI1) .NE. (/10, 1 /)))        STOP 12
  IF (ANY( UBOUND(PtrI1) .NE. (/19, 9 /)))        STOP 13
  IF (ANY( RESHAPE(PtrI1,(/90/))        .NE. PtrI1Tar(1:90,1)))  STOP 14

  PtrI2(10:19, 1:9 ) => PtrI2Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrI2))                    STOP 21
  IF (ANY( LBOUND(PtrI2) .NE. (/10, 1 /)))        STOP 22
  IF (ANY( UBOUND(PtrI2) .NE. (/19, 9 /)))        STOP 23
  IF (ANY( RESHAPE(PtrI2,(/90/))         .NE. PtrI2Tar(1:90,1)))  STOP 24

  PtrI4(10:19, 1:9 ) => PtrI4Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrI4))                    STOP 31
  IF (ANY( LBOUND(PtrI4) .NE. (/10, 1 /)))        STOP 32
  IF (ANY( UBOUND(PtrI4) .NE. (/19, 9 /)))        STOP 33
  IF (ANY( RESHAPE(PtrI4,(/90/))         .NE. PtrI4Tar(1:90,1)))  STOP 34

  PtrI8(10:19, 1:9 ) => PtrI8Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrI8))                    STOP 41
  IF (ANY( LBOUND(PtrI8) .NE. (/10, 1 /)))        STOP 42
  IF (ANY( UBOUND(PtrI8) .NE. (/19, 9 /)))        STOP 43
  IF (ANY( RESHAPE(PtrI8,(/90/))         .NE. PtrI8Tar(1:90,1)))  STOP 44


  PtrR4(10:19, 1:9 ) => PtrR4Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrR4))                    STOP 51
  IF (ANY( LBOUND(PtrR4) .NE. (/10, 1 /)))        STOP 52
  IF (ANY( UBOUND(PtrR4) .NE. (/19, 9 /)))        STOP 53
  IF (ANY( RESHAPE(PtrR4,(/90/))         .NE. PtrR4Tar(1:90,1)))  STOP 54

  PtrR8(10:19, 1:9 ) => PtrR8Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrR8))                    STOP 61
  IF (ANY( LBOUND(PtrR8) .NE. (/10, 1 /)))        STOP 62
  IF (ANY( UBOUND(PtrR8) .NE. (/19, 9 /)))        STOP 63
  IF (ANY( RESHAPE(PtrR8,(/90/))         .NE. PtrR4Tar(1:90,1)))  STOP 64

  PtrR16(10:19, 1:9 ) => PtrR16Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrR16))                   STOP 71
  IF (ANY( LBOUND(PtrR16) .NE. (/10, 1 /)))       STOP 72
  IF (ANY( UBOUND(PtrR16) .NE. (/19, 9 /)))       STOP 73
  IF (ANY( RESHAPE(PtrR16,(/90/))         .NE. PtrR4Tar(1:90,1))) STOP 74


  PtrC4(10:19, 1:9 ) => PtrC4Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrC4))                    STOP 81
  IF (ANY( LBOUND(PtrC4) .NE. (/10, 1 /)))        STOP 82
  IF (ANY( UBOUND(PtrC4) .NE. (/19, 9 /)))        STOP 83
  IF (ANY( RESHAPE(PtrC4,(/90/))         .NE. PtrC4Tar(1:90,1)))  STOP 84

  PtrC8(10:19, 1:9 ) => PtrC8Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrR8))                    STOP 91
  IF (ANY( LBOUND(PtrC8) .NE. (/10, 1 /)))        STOP 92
  IF (ANY( UBOUND(PtrC8) .NE. (/19, 9 /)))        STOP 93
  IF (ANY( RESHAPE(PtrC8,(/90/))         .NE. PtrC8Tar(1:90,1)))  STOP 94

  PtrC16(10:19, 1:9 ) => PtrC16Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrC16))                   STOP 15
  IF (ANY( LBOUND(PtrC16) .NE. (/10, 1 /)))       STOP 16
  IF (ANY( UBOUND(PtrC16) .NE. (/19, 9 /)))       STOP 17
  IF (ANY( RESHAPE(PtrC16,(/90/))         .NE. PtrC16Tar(1:90,1)))STOP 18


  PtrL1(10:19, 1:9 ) => PtrL1Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrL1))                     STOP 25
  IF (ANY( LBOUND(PtrL1) .NE. (/10, 1 /)))         STOP 26
  IF (ANY( UBOUND(PtrL1) .NE. (/19, 9 /)))         STOP 27
  IF (ANY( RESHAPE(PtrL1,(/90/))         .NEQV. PtrL1Tar(1:90,1))) STOP 28

  PtrL2(10:19, 1:9 ) => PtrL2Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrL2))                     STOP 35
  IF (ANY( LBOUND(PtrL2) .NE. (/10, 1 /)))         STOP 36
  IF (ANY( UBOUND(PtrL2) .NE. (/19, 9 /)))         STOP 37
  IF (ANY( RESHAPE(PtrL2,(/90/))         .NEQV. PtrL2Tar(1:90,1))) STOP 38

  PtrL4(10:19, 1:9 ) => PtrL4Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrL4))                     STOP 45
  IF (ANY( LBOUND(PtrL4) .NE. (/10, 1 /)))         STOP 46
  IF (ANY( UBOUND(PtrL4) .NE. (/19, 9 /)))         STOP 47
  IF (ANY( RESHAPE(PtrL4,(/90/))         .NEQV. PtrL4Tar(1:90,1))) STOP 48

  PtrL8(10:19, 1:9 ) => PtrL8Tar(:, 1)
  IF (.NOT. ASSOCIATED(PtrL8))                     STOP 55
  IF (ANY( LBOUND(PtrL8) .NE. (/10, 1 /)))         STOP 56
  IF (ANY( UBOUND(PtrL8) .NE. (/19, 9 /)))         STOP 57
  IF (ANY( RESHAPE(PtrL8,(/90/))         .NEQV. PtrL8Tar(1:90,1))) STOP 58




  END


