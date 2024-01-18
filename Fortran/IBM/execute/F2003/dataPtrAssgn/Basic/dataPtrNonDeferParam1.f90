!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrNonDeferParam1.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 06, 2006
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
!*  If data-target is a disassociated pointer, all nondeferred type parameters 
!*  of the declared type of data-pointer-object that correspond to nondeferred type
!*  parameters of data-target shall have the same values as the corresponding
!*  type parameters of data-target
!*  
!*  (not include type paramter for derived type) 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrNonDeferParam1 
  IMPLICIT NONE

  INTEGER          :: I, J
  REAL(8), POINTER :: PtrReal8(:, :)
  REAL(8), POINTER :: Real8(:, :)=>NULL()

  CHARACTER(4), POINTER :: PtrC4(:,:)
  CHARACTER(:), POINTER :: PtrC4Tar(:, :)=>NULL()
  
  CHARACTER(:), POINTER :: PtrC3(:,:)
  CHARACTER(:), POINTER :: PtrC3Tar(:, :)=>NULL()
  
  CHARACTER(2), POINTER :: PtrC2(:,:)
  CHARACTER(2), POINTER :: PtrC2Tar(:, :)=>NULL()
  
  CHARACTER(:), POINTER :: PtrC1(:,:)
  CHARACTER(1), POINTER :: PtrC1Tar(:, :)=>NULL()
 
  ALLOCATE(PtrReal8(1, 1)) 
  PtrReal8(0:, 0:) => Real8
  IF (ASSOCIATED(PtrReal8)) STOP  11  
  
  ALLOCATE(Real8(1025, 1)) 
  PtrReal8(0:1024, 0:-1) => Real8(1, 1:0) 
  IF (.NOT.ASSOCIATED(PtrReal8)) STOP  12
  !DEALLOCATE(PtrReal8) 

  ALLOCATE(PtrC4(1,1))
  PtrC4(0:, 0:) => PtrC4Tar
  IF (ASSOCIATED(PtrC4)) STOP  13 
  !DEALLOCATE(PtrC4)

  ALLOCATE(PtrC4(1,1))
  PtrC4(0:1, 1:0) => PtrC4Tar(1, 1:0)
  IF (ASSOCIATED(PtrC4)) STOP  14 
  !DEALLOCATE(PtrC4)

  ALLOCATE(PtrC3(1,1), SOURCE="123")
  PtrC3(0:, 0:) => PtrC3Tar
  IF (ASSOCIATED(PtrC3)) STOP  23 
  !DEALLOCATE(PtrC3)

  ALLOCATE(CHARACTER(3)::PtrC3Tar(1,1))
  PtrC3(0:1, 1:0) => PtrC3Tar(1, 1:0)
  IF (.NOT.ASSOCIATED(PtrC3)) STOP  24 
  !DEALLOCATE(PtrC3)

  ALLOCATE(PtrC2(1,1))
  PtrC2(0:, 0:) => PtrC2Tar
  IF (ASSOCIATED(PtrC2)) STOP  15 
  !DEALLOCATE(PtrC2)

  ALLOCATE(PtrC2Tar(1,1))
  PtrC2(0:1, 1:0) => PtrC2Tar(1, 1:0)
  IF (.NOT. ASSOCIATED(PtrC2)) STOP  16 
  !DEALLOCATE(PtrC2)

  ALLOCATE(PtrC1(1,1), SOURCE="1")
  PtrC1(0:, 0:) => PtrC1Tar
  IF (ASSOCIATED(PtrC1)) STOP  17 
  !DEALLOCATE(PtrC1)

  ALLOCATE(CHARACTER(1)::PtrC1Tar(1,1))
  PtrC1(0:1, 1:0) => PtrC1Tar(1, 1:0)
  IF (.NOT. ASSOCIATED(PtrC1)) STOP  18 
  !DEALLOCATE(PtrC1)


  END


