!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrPtrComp.f
!*
!*  DATE                       : Feb. 08, 2006
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
!*  pointer component
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    SEQUENCE
    INTEGER(1),  PRIVATE, POINTER :: PtrI1(:, :)
    REAL(4),     PRIVATE, POINTER :: PtrR4(:, :)
    COMPLEX(4),  PRIVATE, POINTER :: PtrC4(:, :)
    LOGICAL(1),  PRIVATE, POINTER :: PtrL1(:, :)
    CHARACTER(1),PUBLIC,  POINTER :: PtrC1(:, :)

    INTEGER(1)     :: I1Tar(10,10)=1_1
    REAL(4)        :: R4Tar(10,10)=4.0_4
    COMPLEX(4)     :: C4Tar(10,10)=(4.0,-4.0)
    LOGICAL(1)     :: L1Tar(10,10)=.TRUE._1
    CHARACTER(1)   :: C1Tar(10,10)="1"
  END TYPE

  TYPE (DT), SAVE, TARGET   :: T

  CONTAINS

  SUBROUTINE S()

  T%PtrI1(0:, 0: ) => T%I1Tar
  IF (.NOT. ASSOCIATED(T%PtrI1))                       STOP 11
  IF (ANY( LBOUND(T%PtrI1)         .NE. (/0, 0 /)))    STOP 12
  IF (ANY( UBOUND(T%PtrI1)         .NE. (/9, 9 /)))    STOP 13
  IF (ANY( T%PtrI1                 .NE. 1_1))          STOP 14

  T%PtrI1(0:9, 0:0 ) => T%I1Tar(:, 1)
  IF (.NOT. ASSOCIATED(T%PtrI1))                       STOP 15
  IF (ANY( LBOUND(T%PtrI1)         .NE. (/0, 0 /)))    STOP 16
  IF (ANY( UBOUND(T%PtrI1)         .NE. (/9, 0 /)))    STOP 17
  IF (ANY( T%PtrI1                 .NE. 1_1))          STOP 18

  T%PtrR4(0:, 0: ) => T%R4Tar
  IF (.NOT. ASSOCIATED(T%PtrR4))                       STOP 21
  IF (ANY( LBOUND(T%PtrR4)         .NE. (/0, 0 /)))    STOP 22
  IF (ANY( UBOUND(T%PtrR4)         .NE. (/9, 9 /)))    STOP 23
  IF (ANY( T%PtrR4                 .NE. 4.0_4))        STOP 24

  T%PtrR4(0:9, 0:0 ) => T%R4Tar(:, 1)
  IF (.NOT. ASSOCIATED(T%PtrR4))                       STOP 25
  IF (ANY( LBOUND(T%PtrR4)         .NE. (/0, 0 /)))    STOP 26
  IF (ANY( UBOUND(T%PtrR4)         .NE. (/9, 0 /)))    STOP 27
  IF (ANY( T%PtrR4                 .NE. 4.0_4))        STOP 28

  T%PtrC4(0:, 0: ) => T%C4Tar
  IF (.NOT. ASSOCIATED(T%Ptrc4))                       STOP 31
  IF (ANY( LBOUND(T%PtrC4)         .NE. (/0, 0 /)))    STOP 32
  IF (ANY( UBOUND(T%PtrC4)         .NE. (/9, 9 /)))    STOP 33
  IF (ANY( T%PtrC4                 .NE. (4.0,-4.0)))   STOP 34

  T%PtrC4(0:9, 0:0 ) => T%C4Tar(:, 1)
  IF (.NOT. ASSOCIATED(T%PtrC4))                       STOP 35
  IF (ANY( LBOUND(T%PtrC4)         .NE. (/0, 0 /)))    STOP 36
  IF (ANY( UBOUND(T%PtrC4)         .NE. (/9, 0 /)))    STOP 37
  IF (ANY( T%PtrC4                 .NE. (4.0,-4.0)))   STOP 38

  T%PtrL1(0:, 0: ) => T%L1Tar
  IF (.NOT. ASSOCIATED(T%PtrL1))                       STOP 41
  IF (ANY( LBOUND(T%PtrL1)         .NE. (/0, 0 /)))    STOP 42
  IF (ANY( UBOUND(T%PtrL1)         .NE. (/9, 9 /)))    STOP 43
  IF (ANY( T%PtrL1                 .NEQV. .TRUE._1))   STOP 44

  T%PtrL1(0:9, 0:0 ) => T%L1Tar(:, 1)
  IF (.NOT. ASSOCIATED(T%PtrL1))                       STOP 45
  IF (ANY( LBOUND(T%PtrL1)         .NE. (/0, 0 /)))    STOP 46
  IF (ANY( UBOUND(T%PtrL1)         .NE. (/9, 0 /)))    STOP 47
  IF (ANY( T%PtrL1                 .NEQV. .TRUE._1))   STOP 48

  T%PtrC1(0:, 0: ) => T%C1Tar
  IF (.NOT. ASSOCIATED(T%PtrC1))                       STOP 51
  IF (ANY( LBOUND(T%PtrC1)         .NE. (/0, 0 /)))    STOP 52
  IF (ANY( UBOUND(T%PtrC1)         .NE. (/9, 9 /)))    STOP 53
  IF (ANY( T%PtrC1                 .NE. "1"))          STOP 54

  T%PtrC1(0:9, 0:0 ) => T%C1Tar(:, 1)
  IF (.NOT. ASSOCIATED(T%PtrC1))                       STOP 55
  IF (ANY( LBOUND(T%PtrC1)         .NE. (/0, 0 /)))    STOP 56
  IF (ANY( UBOUND(T%PtrC1)         .NE. (/9, 0 /)))    STOP 57
  IF (ANY( T%PtrC1                 .NE. "1"))          STOP 58


  END SUBROUTINE

  END MODULE

  PROGRAM dataPtrPtrComp
  USE M
  IMPLICIT NONE

  CALL S()

  END


