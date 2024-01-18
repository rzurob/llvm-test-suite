! GB DTP extension using:
! ftcx_dtp -qck -qreuse=self /tstdev/F2003/dataPtrAssgn/Basic/dataPtrNonDeferParam2.f
! opt variations: -qnock -qreuse=none

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
!*  -- disassociated
!*  (not include type paramter for derived type)
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrNonDeferParam2
  IMPLICIT NONE

  TYPE :: DT(K1,N1,N2,N3)    ! (1,4,2,1)
    INTEGER, KIND                      :: K1
    INTEGER, LEN                       :: N1,N2,N3
    CHARACTER(kind=K1,len=N1), POINTER :: PtrC4(:,:)
    CHARACTER(:), POINTER :: PtrC4Tar(:, :)=>NULL()

    CHARACTER(:), POINTER :: PtrC3(:,:)
    CHARACTER(:), POINTER :: PtrC3Tar(:, :)=>NULL()

    CHARACTER(kind=K1,len=N2), POINTER :: PtrC2(:,:)
    CHARACTER(kind=K1,len=N2), POINTER :: PtrC2Tar(:, :)=>NULL()

    CHARACTER(:), POINTER :: PtrC1(:,:)
    CHARACTER(kind=K1,len=N3), POINTER :: PtrC1Tar(:, :)=>NULL()
  END TYPE

  TYPE (DT(1,4,2,1)) :: T


  ALLOCATE(T%PtrC4(1,1))
  T%PtrC4(0:, 0:) => T%PtrC4Tar
  IF (ASSOCIATED(T%PtrC4)) ERROR STOP  11
! DEALLOCATE(T%PtrC4)

  ALLOCATE(T%PtrC4(1,1))
  T%PtrC4(0:1, 1:0) => T%PtrC4Tar(1, 1:0)
  IF (ASSOCIATED(T%PtrC4)) ERROR STOP  12
! DEALLOCATE(T%PtrC4)

  ALLOCATE(CHARACTER(3) :: T%PtrC3(1,1))
  T%PtrC3(0:, 0:) => T%PtrC3Tar
  IF (ASSOCIATED(T%PtrC3)) ERROR STOP  13
! DEALLOCATE(T%PtrC3)

  T%PtrC3(0:1, 1:0) => T%PtrC3Tar(1, 1:0)
  IF (ASSOCIATED(T%PtrC3)) ERROR STOP  14
! DEALLOCATE(T%PtrC3)

  ALLOCATE(T%PtrC2(1,1))
  T%PtrC2(0:, 0:) => T%PtrC2Tar
  IF (ASSOCIATED(T%PtrC2)) ERROR STOP  15
! DEALLOCATE(T%PtrC2)

  ALLOCATE(T%PtrC2(1,1))
  T%PtrC2(0:1, 1:0) => T%PtrC2Tar(1, 1:0)
  IF (ASSOCIATED(T%PtrC2)) ERROR STOP  16
! DEALLOCATE(T%PtrC2)

  ALLOCATE(CHARACTER::T%PtrC1(1,1))
  T%PtrC1(0:, 0:) => T%PtrC1Tar
  IF (ASSOCIATED(T%PtrC1)) ERROR STOP  17
! DEALLOCATE(T%PtrC1)

  T%PtrC1(0:1, 1:0) => T%PtrC1Tar(1, 1:0)
  IF (ASSOCIATED(T%PtrC1)) ERROR STOP  18
! DEALLOCATE(T%PtrC1)


  END


