!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrSyntax2.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax 
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
!*  Syntax checking:
!* 
!*  R735 pointer-assignment-stmt is 
!*    data-pointer-object (bounds-remapping-list ) => data-target 
!*  R738 bounds-remapping is lower-bound-expr : upper-bound-expr 
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrSyntax2
  IMPLICIT NONE

  TYPE :: DT0
  END TYPE
  
  TYPE(DT0), TARGET  :: Arr1(-100:-1)
  TYPE(DT0), TARGET  :: Arr2(-9:0, 10)
  TYPE(DT0), POINTER :: Ptr1(:), Ptr2(:,:) 



  Ptr1(:) => Arr1

  Ptr1(:0) => Arr1

  ! these two are ok
  Ptr1(0:0) => Arr1
  Ptr1(1:0) => Arr1


  Ptr2(1:2,1:) => Arr2

  Ptr2(1:, :) => Arr2

  Ptr2(1:, :10) => Arr2

  Ptr2(:, 1:) => Arr2

  Ptr2(:10, :10) => Arr2

  Ptr2(:1,1:) => Arr2


  END


