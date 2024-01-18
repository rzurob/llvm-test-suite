! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/dataPtrAssgn/Basic/dataPtrNonDeferParam3.f
! opt variations: -qnock -qreuse=self

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

  TYPE :: DT(K1,N1,K2,N2,K3,N3,K4,N4)    ! (1,4,1,2,1,2,1,1)
    INTEGER, KIND                      :: K1,K2,K3,K4
    INTEGER, LEN                       :: N1,N2,N3,N4
    CHARACTER(kind=K1,len=N1), POINTER :: PtrC4(:,:) =>NULL()
    CHARACTER(:), POINTER :: PtrC4Tar(:, :)

    CHARACTER(:), POINTER :: PtrC3(:,:) =>NULL()
    CHARACTER(:), POINTER :: PtrC3Tar(:, :)

    CHARACTER(kind=K2,len=N2), POINTER :: PtrC2(:,:) =>NULL()
    CHARACTER(kind=K3,len=N3), POINTER :: PtrC2Tar(:, :)

    CHARACTER(:), POINTER :: PtrC1(:,:) =>NULL()
    CHARACTER(kind=K4,len=N4), POINTER :: PtrC1Tar(:, :)
  END TYPE

  TYPE (DT(1,4,1,2,1,2,1,1)) :: T


  ALLOCATE(T%PtrC4Tar(100, 100), SOURCE="1234")
  T%PtrC4(0:, 0:) => T%PtrC4Tar
  IF (.NOT. ASSOCIATED(T%PtrC4))             ERROR STOP 10
  IF (ANY( LBOUND(T%PtrC4) .NE. (/0,   0/))) ERROR STOP 11
  IF (ANY( UBOUND(T%PtrC4) .NE. (/99, 99/))) ERROR STOP 12
  IF (ANY( T%PtrC4         .NE. "1234"))     ERROR STOP 13
  DEALLOCATE(T%PtrC4Tar)

  ALLOCATE(T%PtrC4Tar(100, 100), SOURCE="1234")
  T%PtrC4(91:99, 91:99) => T%PtrC4Tar(:,1)
  IF (.NOT. ASSOCIATED(T%PtrC4))             ERROR STOP 15
  IF (ANY( LBOUND(T%PtrC4) .NE. (/91, 91/))) ERROR STOP 16
  IF (ANY( UBOUND(T%PtrC4) .NE. (/99, 99/))) ERROR STOP 17
  IF (ANY( T%PtrC4         .NE. "1234"))     ERROR STOP 18
  DEALLOCATE(T%PtrC4Tar)

  ALLOCATE(T%PtrC3Tar(100, 100), SOURCE="123")
  T%PtrC3(0:, 0:) => T%PtrC3Tar
  IF (.NOT. ASSOCIATED(T%PtrC3))             ERROR STOP 20
  IF (ANY( LBOUND(T%PtrC3) .NE. (/0,   0/))) ERROR STOP 21
  IF (ANY( UBOUND(T%PtrC3) .NE. (/99, 99/))) ERROR STOP 22
  IF (ANY( T%PtrC3         .NE. "123"))      ERROR STOP 23
  DEALLOCATE(T%PtrC3Tar)

  ALLOCATE(T%PtrC3Tar(100, 100), SOURCE="123")
  T%PtrC3(91:99, 91:99) => T%PtrC3Tar(:,1)
  IF (.NOT. ASSOCIATED(T%PtrC3))             ERROR STOP 25
  IF (ANY( LBOUND(T%PtrC3) .NE. (/91, 91/))) ERROR STOP 26
  IF (ANY( UBOUND(T%PtrC3) .NE. (/99, 99/))) ERROR STOP 27
  IF (ANY( T%PtrC3         .NE. "123"))      ERROR STOP 28
  DEALLOCATE(T%PtrC3Tar)

  ALLOCATE(T%PtrC2Tar(100, 100), SOURCE="12")
  T%PtrC2(0:, 0:) => T%PtrC2Tar
  IF (.NOT. ASSOCIATED(T%PtrC2))             ERROR STOP 30
  IF (ANY( LBOUND(T%PtrC2) .NE. (/0,   0/))) ERROR STOP 31
  IF (ANY( UBOUND(T%PtrC2) .NE. (/99, 99/))) ERROR STOP 32
  IF (ANY( T%PtrC2         .NE. "12"))       ERROR STOP 33
  DEALLOCATE(T%PtrC2Tar)

  ALLOCATE(T%PtrC2Tar(100, 100), SOURCE="12")
  T%PtrC2(91:99, 91:99) => T%PtrC2Tar(:,1)
  IF (.NOT. ASSOCIATED(T%PtrC2))             ERROR STOP 35
  IF (ANY( LBOUND(T%PtrC2) .NE. (/91, 91/))) ERROR STOP 36
  IF (ANY( UBOUND(T%PtrC2) .NE. (/99, 99/))) ERROR STOP 37
  IF (ANY( T%PtrC2         .NE. "12"))       ERROR STOP 38
  DEALLOCATE(T%PtrC2Tar)

  ALLOCATE(T%PtrC1Tar(100, 100), SOURCE="1")
  T%PtrC1(0:, 0:) => T%PtrC1Tar
  IF (.NOT. ASSOCIATED(T%PtrC1))             ERROR STOP 40
  IF (ANY( LBOUND(T%PtrC1) .NE. (/0,   0/))) ERROR STOP 41
  IF (ANY( UBOUND(T%PtrC1) .NE. (/99, 99/))) ERROR STOP 42
  IF (ANY( T%PtrC1         .NE. "1"))        ERROR STOP 43
  DEALLOCATE(T%PtrC1Tar)

  ALLOCATE(T%PtrC1Tar(100, 100), SOURCE="1")
  T%PtrC1(91:99, 91:99) => T%PtrC1Tar(:,1)
  IF (.NOT. ASSOCIATED(T%PtrC1))             ERROR STOP 45
  IF (ANY( LBOUND(T%PtrC1) .NE. (/91, 91/))) ERROR STOP 46
  IF (ANY( UBOUND(T%PtrC1) .NE. (/99, 99/))) ERROR STOP 47
  IF (ANY( T%PtrC1         .NE. "1"))        ERROR STOP 48
  DEALLOCATE(T%PtrC1Tar)



  END


