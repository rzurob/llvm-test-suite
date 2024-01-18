!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrSyntax1.f  
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
!*     data-pointer-object [ (bounds-spec-list) ] => data-target 
!*  R737 bounds-spec is lower-bound-expr : 
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrSyntax1
  IMPLICIT NONE

  COMPLEX, TARGET  :: CArr1(0:99)
  COMPLEX, TARGET  :: CArr2(0:9, 0:9)

  TYPE :: DT
    COMPLEX, POINTER :: IPtr1(:), IPtr2(:,:)
  END TYPE

  TYPE(DT) :: T
  TYPE(DT), POINTER :: T1(:)


  T%IPtr1() => CArr1

  T%IPtr1(:) => CArr1

  T%IPtr1(:10) => CArr1

  T%IPtr1(1:,1:) => CArr2


  T%IPtr2(1:) => CArr2

  T%IPtr2(1:, :) => CArr2

  T%IPtr2(1:, 0) => CArr1

  T%IPtr2(1:, :10) => CArr2

  T%IPtr2(:, 1:) => CArr2

  T%IPtr2(:10, :10) => CArr2

  T%IPtr2(1, 1:10) => CArr2

  T%IPtr2(1,1:) => CArr2

  T1(1:)%IPtr1 => CArr1


  END


