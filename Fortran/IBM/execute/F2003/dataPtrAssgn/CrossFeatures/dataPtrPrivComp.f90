!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrPrivComp.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 08, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  private components 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    INTEGER(1),  PRIVATE :: PtrI1Tar(10,  10)=-1_1
    INTEGER(2),  PRIVATE :: PtrI2Tar(10,  10)=-2_2
    INTEGER(4),  PRIVATE :: PtrI4Tar(10,  10)=-4_4
    INTEGER(8),  PUBLIC  :: PtrI8Tar(100, 100)=-8_8
  
    REAL(4),     PRIVATE :: PtrR4Tar(10,   10)=-4.0_4
    REAL(8),     PRIVATE :: PtrR8Tar(10,   10)=-8.0_8
    REAL(16),    PUBLIC  :: PtrR16Tar(100, 100)=-16.0_16
  
    COMPLEX(4),  PRIVATE :: PtrC4Tar(10,   10)=(4.0_4, -4.0_4)
    COMPLEX(8),  PRIVATE :: PtrC8Tar(10,   10)=(8.0_8, -8.0_8)
    COMPLEX(16), PUBLIC  :: PtrC16Tar(100, 100)=(16.0_16, -16.0_16)
  
    LOGICAL(1),  PRIVATE :: PtrL1Tar(10, 10)=.TRUE._1
    LOGICAL(2),  PRIVATE :: PtrL2Tar(10, 10)=.TRUE._2
    LOGICAL(4),  PRIVATE :: PtrL4Tar(10, 10)=.TRUE._4
    LOGICAL(8),  PUBLIC  :: PtrL8Tar(10, 10)=.TRUE._8
  
    CHARACTER(1),PUBLIC  :: PtrC1Tar(100, 100)= "1"
  END TYPE


  END MODULE

  PROGRAM dataPtrPrivComp 
  USE M
  IMPLICIT NONE

  INTEGER(8),   POINTER  :: PtrI8(:,:) =>NULL()
  REAL(16),     POINTER  :: PtrR16(:,:) =>NULL()
  COMPLEX(16),  POINTER  :: PtrC16(:,:)=>NULL()
  LOGICAL(8),   POINTER  :: PtrL8(:,:) =>NULL()
  CHARACTER(1), POINTER  :: PtrC1(:,:) =>NULL() 


  TYPE (DT),    POINTER  :: P(:, :) 
  TYPE (DT),    TARGET   :: T(10, 10)

 
  P(0:, 0: ) => T
  IF (.NOT. ASSOCIATED(P))                       STOP 11
  IF (ANY( LBOUND(P)         .NE. (/0, 0 /)))    STOP 12
  IF (ANY( UBOUND(P)         .NE. (/9, 9 /)))    STOP 13
  IF (ANY( P(0,0)%PtrI8Tar   .NE. -8_8))         STOP 14
  IF (ANY( P(0,0)%PtrR16Tar  .NE. -16.0_16))     STOP 15

  P(0:9, 9:9 ) => T(9,:)
  IF (.NOT. ASSOCIATED(P))                       STOP 21
  IF (ANY( LBOUND(P)         .NE. (/0, 9 /)))    STOP 22
  IF (ANY( UBOUND(P)         .NE. (/9, 9 /)))    STOP 23
  IF (ANY( P(0,9)%PtrC16Tar  .NE. (16.0_16, -16.0_16)))     STOP 24
  IF (ANY( P(0,9)%PtrL8Tar   .NEQV. .TRUE._8))   STOP 25


  PtrI8(10:19, 1:9 ) => T(1,1)%PtrI8Tar(:, 10)
  IF (.NOT. ASSOCIATED(PtrI8))                    STOP 41
  IF (ANY( LBOUND(PtrI8) .NE. (/10, 1 /)))        STOP 42
  IF (ANY( UBOUND(PtrI8) .NE. (/19, 9 /)))        STOP 43
  IF (ANY( PtrI8         .NE. -8_8))              STOP 44

  PtrR16(10:19, 1:9 ) => T(2,2)%PtrR16Tar(:, 10)
  IF (.NOT. ASSOCIATED(PtrR16))                   STOP 71
  IF (ANY( LBOUND(PtrR16) .NE. (/10, 1 /)))       STOP 72
  IF (ANY( UBOUND(PtrR16) .NE. (/19, 9 /)))       STOP 73
  IF (ANY( PtrR16         .NE. -16.0_16))         STOP 74

  PtrC16(10:19, 1:9 ) => T(3,3)%PtrC16Tar(:, 10)
  IF (.NOT. ASSOCIATED(PtrC16))                   STOP 35
  IF (ANY( LBOUND(PtrC16) .NE. (/10, 1 /)))       STOP 36
  IF (ANY( UBOUND(PtrC16) .NE. (/19, 9 /)))       STOP 37
  IF (ANY( PtrC16         .NE. (16.0_16, -16.0_16 )))    STOP 38

  PtrL8(10:19, 9:9 ) => T(4,4)%PtrL8Tar(:, 10)
  IF (.NOT. ASSOCIATED(PtrL8))                    STOP 55
  IF (ANY( LBOUND(PtrL8) .NE. (/10, 9 /)))        STOP 56
  IF (ANY( UBOUND(PtrL8) .NE. (/19, 9 /)))        STOP 57
  IF (ANY( PtrL8         .NEQV. .TRUE._8))        STOP 58

  PtrC1(10:19, 1:9 ) => T(5,5)%PtrC1Tar(:, 10)
  IF (.NOT. ASSOCIATED(PtrC1))                    STOP 65
  IF (ANY( LBOUND(PtrC1) .NE. (/10, 1 /)))        STOP 66
  IF (ANY( UBOUND(PtrC1) .NE. (/19, 9 /)))        STOP 67
  IF (ANY( PtrC1         .NE. "1" ))              STOP 68


  END


