!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC723.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 02, 2006
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
!*  C723 (R739) A variable shall have either the TARGET or POINTER attribute, and 
!*  shall not be an  array section with a vector subscript.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC723
  IMPLICIT NONE
  
  INTEGER, ALLOCATABLE :: IAlloc(:)
  INTEGER, POINTER     :: PtrArr(:)
  INTEGER              :: Arr(10) 
  INTEGER, TARGET      :: TarArr(10) 
  INTEGER              :: I, J
  INTEGER, POINTER     :: Ptr(:)

  Ptr(I:) => IAlloc 
  Ptr(I:J) => IAlloc 

  Ptr(I:) => Arr(I:J) 
  Ptr(I:J) => Arr(I:J) 

  Ptr(I:) => TarArr(TarArr) 
  Ptr(I:J) => TarArr(TarArr) 
  
  END


