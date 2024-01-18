! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/Basic/dataPtrSyntax2.f
! opt variations: -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrSyntax2.f
!*
!*  DATE                       : Jan. 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax
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

  TYPE :: DT0(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  TYPE(DT0(4,20)), TARGET  :: Arr1(-100:-1)
  TYPE(DT0(4,20)), TARGET  :: Arr2(-9:0, 10)
  TYPE(DT0(4,:)), POINTER :: Ptr1(:), Ptr2(:,:)



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


