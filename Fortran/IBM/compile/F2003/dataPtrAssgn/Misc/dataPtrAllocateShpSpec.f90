!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrAllocateShpSpec.f
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
!*  diag on allocate shape spec
!*
!*  The problem found in TCD
!*  (323393)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrAllocateShpSpec
  IMPLICIT NONE
  INTEGER, POINTER :: Ptr(:)

  ALLOCATE(Ptr(10:))

  END



