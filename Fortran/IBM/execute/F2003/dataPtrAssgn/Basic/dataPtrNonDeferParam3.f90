!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrNonDeferParam3.f
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
!*  If data-pointer-object has nondeferred type parameters that correspond to deferred
!*  type parameters of data-target, data-target shall not be a pointer
!*  with undefined association status.
!*
!*  -- associated
!*  (not include type paramter for derived type)
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrNonDeferParam3
  IMPLICIT NONE

  TYPE :: DT
    CHARACTER(4), POINTER :: PtrC4(:,:) =>NULL()
    CHARACTER(:), POINTER :: PtrC4Tar(:, :)

    CHARACTER(:), POINTER :: PtrC3(:,:) =>NULL()
    CHARACTER(:), POINTER :: PtrC3Tar(:, :)

    CHARACTER(2), POINTER :: PtrC2(:,:) =>NULL()
    CHARACTER(2), POINTER :: PtrC2Tar(:, :)

    CHARACTER(:), POINTER :: PtrC1(:,:) =>NULL()
    CHARACTER(1), POINTER :: PtrC1Tar(:, :)
  END TYPE

  TYPE (DT) :: T


  ALLOCATE(T%PtrC4Tar(100, 100), SOURCE="1234")
  T%PtrC4(0:, 0:) => T%PtrC4Tar
  IF (.NOT. ASSOCIATED(T%PtrC4))             STOP 10
  IF (ANY( LBOUND(T%PtrC4) .NE. (/0,   0/))) STOP 11
  IF (ANY( UBOUND(T%PtrC4) .NE. (/99, 99/))) STOP 12
  IF (ANY( T%PtrC4         .NE. "1234"))     STOP 13
  DEALLOCATE(T%PtrC4Tar)

  ALLOCATE(T%PtrC4Tar(100, 100), SOURCE="1234")
  T%PtrC4(91:99, 91:99) => T%PtrC4Tar(:,1)
  IF (.NOT. ASSOCIATED(T%PtrC4))             STOP 15
  IF (ANY( LBOUND(T%PtrC4) .NE. (/91, 91/))) STOP 16
  IF (ANY( UBOUND(T%PtrC4) .NE. (/99, 99/))) STOP 17
  IF (ANY( T%PtrC4         .NE. "1234"))     STOP 18
  DEALLOCATE(T%PtrC4Tar)

  ALLOCATE(T%PtrC3Tar(100, 100), SOURCE="123")
  T%PtrC3(0:, 0:) => T%PtrC3Tar
  IF (.NOT. ASSOCIATED(T%PtrC3))             STOP 20
  IF (ANY( LBOUND(T%PtrC3) .NE. (/0,   0/))) STOP 21
  IF (ANY( UBOUND(T%PtrC3) .NE. (/99, 99/))) STOP 22
  IF (ANY( T%PtrC3         .NE. "123"))      STOP 23
  DEALLOCATE(T%PtrC3Tar)

  ALLOCATE(T%PtrC3Tar(100, 100), SOURCE="123")
  T%PtrC3(91:99, 91:99) => T%PtrC3Tar(:,1)
  IF (.NOT. ASSOCIATED(T%PtrC3))             STOP 25
  IF (ANY( LBOUND(T%PtrC3) .NE. (/91, 91/))) STOP 26
  IF (ANY( UBOUND(T%PtrC3) .NE. (/99, 99/))) STOP 27
  IF (ANY( T%PtrC3         .NE. "123"))      STOP 28
  DEALLOCATE(T%PtrC3Tar)

  ALLOCATE(T%PtrC2Tar(100, 100), SOURCE="12")
  T%PtrC2(0:, 0:) => T%PtrC2Tar
  IF (.NOT. ASSOCIATED(T%PtrC2))             STOP 30
  IF (ANY( LBOUND(T%PtrC2) .NE. (/0,   0/))) STOP 31
  IF (ANY( UBOUND(T%PtrC2) .NE. (/99, 99/))) STOP 32
  IF (ANY( T%PtrC2         .NE. "12"))       STOP 33
  DEALLOCATE(T%PtrC2Tar)

  ALLOCATE(T%PtrC2Tar(100, 100), SOURCE="12")
  T%PtrC2(91:99, 91:99) => T%PtrC2Tar(:,1)
  IF (.NOT. ASSOCIATED(T%PtrC2))             STOP 35
  IF (ANY( LBOUND(T%PtrC2) .NE. (/91, 91/))) STOP 36
  IF (ANY( UBOUND(T%PtrC2) .NE. (/99, 99/))) STOP 37
  IF (ANY( T%PtrC2         .NE. "12"))       STOP 38
  DEALLOCATE(T%PtrC2Tar)

  ALLOCATE(T%PtrC1Tar(100, 100), SOURCE="1")
  T%PtrC1(0:, 0:) => T%PtrC1Tar
  IF (.NOT. ASSOCIATED(T%PtrC1))             STOP 40
  IF (ANY( LBOUND(T%PtrC1) .NE. (/0,   0/))) STOP 41
  IF (ANY( UBOUND(T%PtrC1) .NE. (/99, 99/))) STOP 42
  IF (ANY( T%PtrC1         .NE. "1"))        STOP 43
  DEALLOCATE(T%PtrC1Tar)

  ALLOCATE(T%PtrC1Tar(100, 100), SOURCE="1")
  T%PtrC1(91:99, 91:99) => T%PtrC1Tar(:,1)
  IF (.NOT. ASSOCIATED(T%PtrC1))             STOP 45
  IF (ANY( LBOUND(T%PtrC1) .NE. (/91, 91/))) STOP 46
  IF (ANY( UBOUND(T%PtrC1) .NE. (/99, 99/))) STOP 47
  IF (ANY( T%PtrC1         .NE. "1"))        STOP 48
  DEALLOCATE(T%PtrC1Tar)



  END


