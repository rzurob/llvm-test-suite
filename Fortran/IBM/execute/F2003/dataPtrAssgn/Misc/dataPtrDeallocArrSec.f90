!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 17, 2006
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
!*  diag on deallocation of pointer arrays that are associated with array sections
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrDeallocArrSec
  IMPLICIT NONE


  INTEGER, ALLOCATABLE, TARGET ::  Tar(:)
  INTEGER, POINTER :: Ptr(:,:)


  ALLOCATE(Tar(10))
  Ptr(1:5, 1:2) => Tar(1:)
  PRINT*, "DEALLOCATING this shall fail!"
  DEALLOCATE(Ptr)


  END



